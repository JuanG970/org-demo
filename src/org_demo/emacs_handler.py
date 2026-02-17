"""Interface for running elisp code against an Emacs daemon.

Follows the same architecture as org-warrior's EmacsHandler: loads .el files
from a sibling ``elisp/`` directory, substitutes parameters, and evaluates
them via ``emacsclient``.
"""

from emacs import EmacsClient
from emacs.elisp import Raw
from pathlib import Path
from dataclasses import dataclass, field
from typing import Any, Optional
import json
import os
import re


@dataclass
class EmacsResponse:
    """Structured response from an Emacs evaluation."""

    success: bool
    data: Any
    error: str = ""


class EmacsHandler:
    """Load and run elisp files/expressions against a running Emacs server.

    Parameters
    ----------
    daemon
        Name of the Emacs server (``emacsclient -s <daemon>``).
        When *None*, the default server is used.
    """

    ELISP_DIR = Path(__file__).parent / "elisp"

    def __init__(self, daemon: Optional[str] = None):
        self.daemon = daemon
        self.client = EmacsClient(server=daemon)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _load_elisp(self, filename: str) -> str:
        """Read an elisp file from the package's ``elisp/`` directory."""
        path = self.ELISP_DIR / filename
        try:
            return path.read_text()
        except Exception as e:
            raise RuntimeError(f"Error loading elisp file {filename}: {e}")

    @staticmethod
    def _strip_elisp_comments(code: str) -> str:
        """Remove lines that are pure elisp comments (starting with ;;).

        This is needed because the ``emacs`` library's ``eval()`` wraps the
        code inside ``json-encode(condition-case ...)``, which requires a
        single valid sexp — not a file with comment lines.
        """
        lines = code.split("\n")
        stripped = [l for l in lines if not re.match(r"^\s*;;", l)]
        return "\n".join(stripped).strip()

    @staticmethod
    def _parse_emacs_response(response: Any, output_format: str = "string") -> Any:
        """Convert the raw Emacs response into the requested format."""
        if output_format == "string":
            return str(response)
        elif output_format == "json":
            try:
                return json.loads(response)
            except json.JSONDecodeError as e:
                raise ValueError(f"Error parsing JSON response from Emacs: {e}")
        else:
            raise ValueError(f"Unsupported output format: {output_format}")

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def execute_file(
        self,
        filename: str,
        params: Optional[dict] = None,
        output_format: str = "string",
    ) -> EmacsResponse:
        """Run an elisp file, optionally substituting ``{{key}}`` params.

        Parameters
        ----------
        filename
            Name of the ``.el`` file inside the ``elisp/`` directory.
        params
            Dictionary of ``{{key}} -> value`` substitutions.
        output_format
            ``"string"`` or ``"json"``.
        """
        elisp_code = self._load_elisp(filename)
        if params:
            for key, value in params.items():
                # Escape double-quotes and backslashes for safe embedding
                escaped = str(value).replace("\\", "\\\\").replace('"', '\\"')
                elisp_code = elisp_code.replace(f"{{{{{key}}}}}", escaped)
        elisp_code = self._strip_elisp_comments(elisp_code)
        try:
            # When the caller expects JSON, the elisp itself returns a
            # json-encoded string.  Tell the emacs library so it doesn't
            # double-encode via ``json-encode``.
            is_json = output_format == "json"
            result = self.client.eval(Raw(elisp_code), is_json=is_json)
            if is_json:
                # The library already decoded the JSON for us
                return EmacsResponse(success=True, data=result)
            parsed = self._parse_emacs_response(result, output_format)
            return EmacsResponse(success=True, data=parsed)
        except Exception as e:
            return EmacsResponse(success=False, data=None, error=str(e))

    def execute_elisp(self, code: str, output_format: str = "string") -> EmacsResponse:
        """Evaluate a raw elisp expression string."""
        try:
            result = self.client.eval(Raw(code))
            parsed = self._parse_emacs_response(result, output_format)
            return EmacsResponse(success=True, data=parsed)
        except Exception as e:
            return EmacsResponse(success=False, data=None, error=str(e))

    # ------------------------------------------------------------------
    # Convenience class-level access
    # ------------------------------------------------------------------

    @staticmethod
    def run_file(
        filename: str,
        params: Optional[dict] = None,
        output_format: str = "string",
        daemon: Optional[str] = None,
    ) -> EmacsResponse:
        """One-shot static helper – create a handler, run, return."""
        server = daemon or os.environ.get("EMACS_SERVER")
        handler = EmacsHandler(daemon=server)
        return handler.execute_file(filename, params, output_format)
