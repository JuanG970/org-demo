"""CLI for org-demo — a Showboat-compatible tool backed by org-mode.

Usage mirrors Showboat exactly, but the output format is ``.org`` rather
than Markdown, and all document manipulation runs through Emacs elisp.
"""

from __future__ import annotations

import sys
from typing import Optional

import click

from .document import OrgDocument, OrgDemoError
from .emacs_handler import EmacsHandler


def _make_document(workdir: str | None, daemon: str | None) -> OrgDocument:
    handler = EmacsHandler(daemon=daemon)
    return OrgDocument(handler=handler, workdir=workdir)


@click.group()
@click.option(
    "--workdir",
    default=None,
    type=click.Path(exists=True, file_okay=False),
    help="Working directory for code execution (default: current).",
)
@click.option(
    "--daemon",
    default=None,
    envvar="EMACS_SERVER",
    help="Emacs server name (emacsclient -s).",
)
@click.version_option(package_name="org-demo")
@click.pass_context
def cli(ctx: click.Context, workdir: str | None, daemon: str | None) -> None:
    """Create executable demo documents backed by org-mode.

    org-demo is a Python re-implementation of Showboat that uses Emacs
    org-mode as the document backend instead of Markdown.  Every command
    delegates to elisp evaluated against a running Emacs server.
    """
    ctx.ensure_object(dict)
    ctx.obj["workdir"] = workdir
    ctx.obj["daemon"] = daemon


# ------------------------------------------------------------------
# init
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.argument("title")
@click.pass_context
def init(ctx: click.Context, file: str, title: str) -> None:
    """Create a new demo document."""
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        path = doc.init(file, title)
        click.echo(f"Created {path}")
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# note
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.argument("text", required=False)
@click.pass_context
def note(ctx: click.Context, file: str, text: str | None) -> None:
    """Append commentary (text or stdin)."""
    if text is None:
        text = click.get_text_stream("stdin").read().rstrip("\n")
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        doc.note(file, text)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# exec
# ------------------------------------------------------------------


@cli.command("exec")
@click.argument("file")
@click.argument("lang")
@click.argument("code", required=False)
@click.pass_context
def exec_cmd(ctx: click.Context, file: str, lang: str, code: str | None) -> None:
    """Run code, capture output, and append both to the document."""
    if code is None:
        code = click.get_text_stream("stdin").read()
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        output = doc.exec(file, lang, code)
        # Print captured output to stdout (like showboat)
        if output:
            click.echo(output, nl=False)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# image
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.argument("path_or_ref")
@click.pass_context
def image(ctx: click.Context, file: str, path_or_ref: str) -> None:
    """Copy image into document and append a link.

    PATH_OR_REF can be a plain file path or ``![alt](path)``.
    """
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        dest = doc.image(file, path_or_ref)
        click.echo(f"Added image {dest}")
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# pop
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.pass_context
def pop(ctx: click.Context, file: str) -> None:
    """Remove the most recent entry from the document."""
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        doc.pop(file)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# verify
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.option(
    "--output",
    "output_file",
    default=None,
    help="Write an updated copy to this path instead of modifying the original.",
)
@click.pass_context
def verify(ctx: click.Context, file: str, output_file: str | None) -> None:
    """Re-run all code blocks and compare outputs."""
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        all_match, report = doc.verify(file, output_file=output_file)
        click.echo(report)
        if not all_match:
            raise SystemExit(1)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# extract
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.option(
    "--filename",
    default=None,
    help="Substitute a different filename in the emitted commands.",
)
@click.pass_context
def extract(ctx: click.Context, file: str, filename: str | None) -> None:
    """Emit the commands that would recreate the document."""
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        commands = doc.extract(file, filename=filename)
        click.echo(commands)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)
