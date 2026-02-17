"""Tests for org-demo.

These tests mock the EmacsHandler so they can run without a live Emacs
daemon.  They verify the OrgDocument orchestration logic and the CLI
wiring.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from org_demo.emacs_handler import EmacsHandler, EmacsResponse
from org_demo.document import OrgDocument, OrgDemoError


# ======================================================================
# Fixtures
# ======================================================================


@pytest.fixture
def mock_handler():
    """Return an EmacsHandler with a mocked EmacsClient."""
    handler = MagicMock(spec=EmacsHandler)
    handler.execute_file = MagicMock(
        return_value=EmacsResponse(success=True, data="ok")
    )
    return handler


@pytest.fixture
def doc(mock_handler, tmp_path):
    """Return an OrgDocument wired to the mock handler."""
    return OrgDocument(handler=mock_handler, workdir=str(tmp_path))


# ======================================================================
# EmacsHandler unit tests
# ======================================================================


class TestEmacsHandler:
    def test_load_elisp_finds_files(self):
        """The elisp/ directory should contain our .el files."""
        elisp_dir = EmacsHandler.ELISP_DIR
        assert (elisp_dir / "org-demo-init.el").exists()
        assert (elisp_dir / "org-demo-note.el").exists()
        assert (elisp_dir / "org-demo-exec.el").exists()
        assert (elisp_dir / "org-demo-image.el").exists()
        assert (elisp_dir / "org-demo-pop.el").exists()
        assert (elisp_dir / "org-demo-read.el").exists()
        assert (elisp_dir / "org-demo-verify.el").exists()

    def test_load_elisp_missing_file(self):
        """Loading a nonexistent .el file should raise RuntimeError."""
        handler = EmacsHandler.__new__(EmacsHandler)
        with pytest.raises(RuntimeError, match="Error loading elisp"):
            handler._load_elisp("does-not-exist.el")

    def test_parse_response_string(self):
        assert EmacsHandler._parse_emacs_response("hello", "string") == "hello"

    def test_parse_response_json(self):
        result = EmacsHandler._parse_emacs_response('{"a": 1}', "json")
        assert result == {"a": 1}

    def test_parse_response_json_invalid(self):
        with pytest.raises(ValueError, match="Error parsing JSON"):
            EmacsHandler._parse_emacs_response("not json", "json")

    def test_parse_response_unsupported_format(self):
        with pytest.raises(ValueError, match="Unsupported output format"):
            EmacsHandler._parse_emacs_response("x", "xml")


# ======================================================================
# OrgDocument unit tests
# ======================================================================


class TestOrgDocumentInit:
    def test_init_calls_elisp(self, doc, mock_handler):
        doc.init("demo.org", "My Demo")
        mock_handler.execute_file.assert_called_once()
        args = mock_handler.execute_file.call_args
        assert args[0][0] == "org-demo-init.el"
        params = args[1].get("params") or args[0][1]
        assert params["title"] == "My Demo"
        assert "demo.org" in params["file"]

    def test_init_raises_on_failure(self, mock_handler):
        mock_handler.execute_file.return_value = EmacsResponse(
            success=False, data=None, error="boom"
        )
        doc = OrgDocument(handler=mock_handler)
        with pytest.raises(OrgDemoError, match="init failed"):
            doc.init("demo.org", "title")


class TestOrgDocumentNote:
    def test_note_calls_elisp(self, doc, mock_handler):
        doc.note("demo.org", "Some commentary")
        args = mock_handler.execute_file.call_args
        assert args[0][0] == "org-demo-note.el"
        params = args[1].get("params") or args[0][1]
        assert params["text"] == "Some commentary"


class TestOrgDocumentExec:
    def test_exec_calls_elisp(self, doc, mock_handler):
        mock_handler.execute_file.return_value = EmacsResponse(
            success=True, data="hello\n"
        )
        output = doc.exec("demo.org", "bash", "echo hello")
        assert output == "hello\n"

        args = mock_handler.execute_file.call_args
        assert args[0][0] == "org-demo-exec.el"
        params = args[1].get("params") or args[0][1]
        assert params["lang"] == "bash"
        assert params["code"] == "echo hello"

    def test_exec_raises_on_failure(self, mock_handler):
        mock_handler.execute_file.return_value = EmacsResponse(
            success=False, data=None, error="boom"
        )
        doc = OrgDocument(handler=mock_handler)
        with pytest.raises(OrgDemoError, match="exec failed"):
            doc.exec("demo.org", "bash", "false")


class TestOrgDocumentImage:
    def test_parse_image_ref_plain_path(self):
        alt, path = OrgDocument._parse_image_ref("screenshot.png")
        assert alt == ""
        assert path == "screenshot.png"

    def test_parse_image_ref_markdown_style(self):
        alt, path = OrgDocument._parse_image_ref("![My screenshot](shot.png)")
        assert alt == "My screenshot"
        assert path == "shot.png"


class TestOrgDocumentPop:
    def test_pop_calls_elisp(self, doc, mock_handler):
        doc.pop("demo.org")
        args = mock_handler.execute_file.call_args
        assert args[0][0] == "org-demo-pop.el"


class TestOrgDocumentVerify:
    def _make_doc_data(self, output="hi"):
        return {
            "title": "Test",
            "uuid": "abc",
            "date": "[2026-02-17]",
            "entries": [
                {"type": "exec", "lang": "bash", "code": "echo hi", "output": output},
            ],
        }

    def test_verify_matching_outputs(self, mock_handler, tmp_path):
        """When babel re-execution produces the same output, verify passes."""
        # Create a dummy file so shutil.copy2 works
        org_file = tmp_path / "demo.org"
        org_file.write_text("placeholder")

        doc_data = self._make_doc_data("hi")
        call_count = {"read": 0}

        def side_effect(filename, **kwargs):
            if filename == "org-demo-read.el":
                call_count["read"] += 1
                # Both reads return the same data (output matches)
                return EmacsResponse(success=True, data=doc_data)
            elif filename == "org-demo-verify.el":
                return EmacsResponse(success=True, data="ok")
            return EmacsResponse(success=True, data="ok")

        mock_handler.execute_file.side_effect = side_effect
        doc = OrgDocument(handler=mock_handler)
        all_match, report = doc.verify(str(org_file))
        assert all_match
        assert "match" in report.lower()
        # Should read twice: once before, once after babel re-execution
        assert call_count["read"] == 2

    def test_verify_mismatched_outputs(self, mock_handler, tmp_path):
        """When babel re-execution produces different output, verify fails."""
        org_file = tmp_path / "demo.org"
        org_file.write_text("placeholder")

        old_data = self._make_doc_data("hi")
        new_data = self._make_doc_data("different")
        call_count = {"read": 0}

        def side_effect(filename, **kwargs):
            if filename == "org-demo-read.el":
                call_count["read"] += 1
                # First read returns old, second returns new
                if call_count["read"] == 1:
                    return EmacsResponse(success=True, data=old_data)
                else:
                    return EmacsResponse(success=True, data=new_data)
            elif filename == "org-demo-verify.el":
                return EmacsResponse(success=True, data="ok")
            return EmacsResponse(success=True, data="ok")

        mock_handler.execute_file.side_effect = side_effect
        doc = OrgDocument(handler=mock_handler)
        all_match, report = doc.verify(str(org_file))
        assert not all_match
        assert "---" in report or "+++" in report

    def test_verify_with_output_file(self, mock_handler, tmp_path):
        """When --output is given, the original file is not modified."""
        org_file = tmp_path / "demo.org"
        org_file.write_text("original content")
        output_file = tmp_path / "verified.org"

        doc_data = self._make_doc_data("hi")

        def side_effect(filename, **kwargs):
            if filename == "org-demo-read.el":
                return EmacsResponse(success=True, data=doc_data)
            elif filename == "org-demo-verify.el":
                # Verify should be called on the output file, not the original
                params = kwargs.get("params", {})
                assert str(output_file) in params.get("file", "")
                return EmacsResponse(success=True, data="ok")
            return EmacsResponse(success=True, data="ok")

        mock_handler.execute_file.side_effect = side_effect
        doc = OrgDocument(handler=mock_handler)
        all_match, report = doc.verify(str(org_file), output_file=str(output_file))
        assert all_match
        # The copy should exist
        assert output_file.exists()


class TestOrgDocumentExtract:
    def test_extract_emits_commands(self, mock_handler):
        read_response = EmacsResponse(
            success=True,
            data={
                "title": "My Demo",
                "uuid": "abc",
                "date": "[2026-02-17]",
                "entries": [
                    {"type": "note", "text": "Hello world"},
                    {"type": "exec", "lang": "bash", "code": "echo hi", "output": "hi"},
                ],
            },
        )

        def side_effect(filename, **kwargs):
            if filename == "org-demo-read.el":
                return read_response
            return EmacsResponse(success=True, data="ok")

        mock_handler.execute_file.side_effect = side_effect

        doc = OrgDocument(handler=mock_handler)
        commands = doc.extract("demo.org")
        lines = commands.strip().split("\n")
        assert len(lines) == 3
        assert "init" in lines[0]
        assert "My Demo" in lines[0]
        assert "note" in lines[1]
        assert "Hello world" in lines[1]
        assert "exec" in lines[2]
        assert "bash" in lines[2]

    def test_extract_with_custom_filename(self, mock_handler):
        read_response = EmacsResponse(
            success=True,
            data={
                "title": "T",
                "uuid": "x",
                "date": "",
                "entries": [],
            },
        )
        mock_handler.execute_file.return_value = read_response

        doc = OrgDocument(handler=mock_handler)
        commands = doc.extract("demo.org", filename="copy.org")
        assert "copy.org" in commands
        assert "demo.org" not in commands


# ======================================================================
# CLI smoke tests
# ======================================================================


class TestCLI:
    def test_cli_help(self):
        from click.testing import CliRunner
        from org_demo.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["--help"])
        assert result.exit_code == 0
        assert "org-mode" in result.output.lower() or "demo" in result.output.lower()

    def test_cli_init_subcommand_exists(self):
        from click.testing import CliRunner
        from org_demo.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["init", "--help"])
        assert result.exit_code == 0
        assert "FILE" in result.output or "file" in result.output

    def test_cli_exec_subcommand_exists(self):
        from click.testing import CliRunner
        from org_demo.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["exec", "--help"])
        assert result.exit_code == 0

    def test_cli_verify_subcommand_exists(self):
        from click.testing import CliRunner
        from org_demo.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["verify", "--help"])
        assert result.exit_code == 0
        assert "--output" in result.output

    def test_cli_extract_subcommand_exists(self):
        from click.testing import CliRunner
        from org_demo.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["extract", "--help"])
        assert result.exit_code == 0
        assert "--filename" in result.output
