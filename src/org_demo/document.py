"""High-level operations on org-mode demo documents.

Each public method maps 1-to-1 to a Showboat CLI command, but the backend
is org-mode manipulated via Emacs elisp rather than raw markdown.
"""

from __future__ import annotations

import os
import re
import shlex
import shutil
import uuid as _uuid
from difflib import unified_diff
from pathlib import Path
from typing import Optional

from .emacs_handler import EmacsHandler, EmacsResponse


class OrgDemoError(Exception):
    """Raised when an org-demo operation fails."""


class OrgDocument:
    """Facade over a single ``.org`` demo document.

    Parameters
    ----------
    handler
        An :class:`EmacsHandler` to use for elisp evaluation.
    workdir
        Working directory for code execution.  Defaults to ``os.getcwd()``.
    """

    def __init__(
        self,
        handler: Optional[EmacsHandler] = None,
        workdir: Optional[str] = None,
    ):
        self.handler = handler or EmacsHandler()
        self.workdir = workdir or os.getcwd()

    # ------------------------------------------------------------------
    # init
    # ------------------------------------------------------------------

    def init(self, file: str, title: str) -> str:
        """Create a new demo document.

        Returns the absolute path of the created file.
        """
        doc_uuid = str(_uuid.uuid4())
        abs_file = str(Path(file).resolve())
        resp = self.handler.execute_file(
            "org-demo-init.el",
            params={"file": abs_file, "title": title, "uuid": doc_uuid},
        )
        if not resp.success:
            raise OrgDemoError(f"init failed: {resp.error}")
        return abs_file

    # ------------------------------------------------------------------
    # note
    # ------------------------------------------------------------------

    def note(self, file: str, text: str) -> None:
        """Append commentary text to the document."""
        abs_file = str(Path(file).resolve())
        resp = self.handler.execute_file(
            "org-demo-note.el",
            params={"file": abs_file, "text": text},
        )
        if not resp.success:
            raise OrgDemoError(f"note failed: {resp.error}")

    # ------------------------------------------------------------------
    # exec
    # ------------------------------------------------------------------

    def exec(self, file: str, lang: str, code: str) -> str:
        """Append a source block and execute it via org-babel.

        Babel handles language routing and result insertion natively.
        Returns the captured output as a string.
        """
        abs_file = str(Path(file).resolve())
        resp = self.handler.execute_file(
            "org-demo-exec.el",
            params={"file": abs_file, "lang": lang, "code": code},
            output_format="json",
        )
        if not resp.success:
            raise OrgDemoError(f"exec failed: {resp.error}")
        return resp.data

    # ------------------------------------------------------------------
    # image
    # ------------------------------------------------------------------

    def image(self, file: str, image_ref: str) -> str:
        """Copy an image into the document directory and append a link.

        *image_ref* can be a plain path or ``![alt](path)``.
        Returns the destination filename.
        """
        abs_file = str(Path(file).resolve())
        alt_text, image_path = self._parse_image_ref(image_ref)

        ext = Path(image_path).suffix or ".png"
        dest_filename = f"org-demo-{_uuid.uuid4().hex[:8]}{ext}"
        abs_image = str(Path(image_path).resolve())

        resp = self.handler.execute_file(
            "org-demo-image.el",
            params={
                "file": abs_file,
                "image_path": abs_image,
                "alt_text": alt_text,
                "dest_filename": dest_filename,
            },
        )
        if not resp.success:
            raise OrgDemoError(f"image failed: {resp.error}")
        return dest_filename

    # ------------------------------------------------------------------
    # pop
    # ------------------------------------------------------------------

    def pop(self, file: str) -> None:
        """Remove the most recent entry from the document."""
        abs_file = str(Path(file).resolve())
        resp = self.handler.execute_file(
            "org-demo-pop.el",
            params={"file": abs_file},
        )
        if not resp.success:
            raise OrgDemoError(f"pop failed: {resp.error}")

    # ------------------------------------------------------------------
    # verify
    # ------------------------------------------------------------------

    def verify(self, file: str, output_file: Optional[str] = None) -> tuple[bool, str]:
        """Re-execute every code block via org-babel and compare outputs.

        1. Read the document to capture current results.
        2. Copy to output_file if given, otherwise work on the original.
        3. Call org-babel-execute-buffer to re-run all blocks.
        4. Read the document again to capture new results.
        5. Compare old vs new.

        Returns ``(all_match, report_text)``.
        """
        abs_file = str(Path(file).resolve())

        # 1. Read existing results
        old_doc = self._read_document(abs_file)
        old_entries = old_doc.get("entries", [])

        # 2. Determine which file to re-execute on
        if output_file:
            target = str(Path(output_file).resolve())
            shutil.copy2(abs_file, target)
        else:
            target = abs_file

        # 3. Re-execute all blocks via babel
        resp = self.handler.execute_file(
            "org-demo-verify.el",
            params={"file": target},
        )
        if not resp.success:
            raise OrgDemoError(f"verify failed: {resp.error}")

        # 4. Read new results
        new_doc = self._read_document(target)
        new_entries = new_doc.get("entries", [])

        # 5. Compare exec blocks
        diffs: list[str] = []
        block_idx = 0
        for old_entry, new_entry in zip(old_entries, new_entries):
            if old_entry["type"] != "exec":
                continue
            block_idx += 1
            expected = old_entry.get("output", "").rstrip("\n")
            actual = new_entry.get("output", "").rstrip("\n")
            if expected != actual:
                diff = "\n".join(
                    unified_diff(
                        expected.splitlines(),
                        actual.splitlines(),
                        fromfile=f"block {block_idx} (expected)",
                        tofile=f"block {block_idx} (actual)",
                        lineterm="",
                    )
                )
                diffs.append(diff)

        all_match = len(diffs) == 0
        report = "\n\n".join(diffs) if diffs else "All outputs match."
        return all_match, report

    # ------------------------------------------------------------------
    # extract
    # ------------------------------------------------------------------

    def extract(self, file: str, filename: Optional[str] = None) -> str:
        """Emit the sequence of org-demo commands that would recreate *file*.

        Returns the commands as a single string (one per line).
        """
        abs_file = str(Path(file).resolve())
        doc = self._read_document(abs_file)

        target = filename or os.path.basename(file)
        lines: list[str] = []

        lines.append(f"org-demo init {shlex.quote(target)} {shlex.quote(doc['title'])}")

        for entry in doc.get("entries", []):
            if entry["type"] == "note":
                lines.append(
                    f"org-demo note {shlex.quote(target)} {shlex.quote(entry['text'])}"
                )
            elif entry["type"] == "exec":
                lines.append(
                    f"org-demo exec {shlex.quote(target)} {entry['lang']} {shlex.quote(entry['code'])}"
                )
            elif entry["type"] == "image":
                path = entry.get("path", "")
                alt = entry.get("alt", "")
                if alt:
                    lines.append(
                        f"org-demo image {shlex.quote(target)} {shlex.quote(f'![{alt}]({path})')}"
                    )
                else:
                    lines.append(
                        f"org-demo image {shlex.quote(target)} {shlex.quote(path)}"
                    )

        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _read_document(self, abs_file: str) -> dict:
        """Parse the org document into a dict via elisp."""
        resp = self.handler.execute_file(
            "org-demo-read.el",
            params={"file": abs_file},
            output_format="json",
        )
        if not resp.success:
            raise OrgDemoError(f"Failed to read document: {resp.error}")
        return resp.data

    @staticmethod
    def _parse_image_ref(ref: str) -> tuple[str, str]:
        """Parse ``![alt](path)`` or a plain path.

        Returns ``(alt_text, image_path)``.
        """
        md_match = re.match(r"^!\[([^\]]*)\]\(([^)]+)\)$", ref)
        if md_match:
            return md_match.group(1), md_match.group(2)
        return "", ref
