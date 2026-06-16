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


_MAIN_HELP = """\
Create executable demo documents backed by org-mode.

org-demo helps agents build emacs org-mode documents that mix commentary, executable
code blocks, and captured output. These documents serve as both readable
documentation and reproducible proof of work. A verifier can re-execute all
code blocks and confirm the outputs still match.

Every command delegates to elisp evaluated against a running Emacs server, and all
code execution is handled by org-babel.

\b
Examples:
  # Create a new document and add some code
  org-demo init demo.org "My Demo"
  org-demo init demo.org "** Before"
  org-demo exec demo.org bash "echo hello"
  org-demo init demo.org "** After"
  org-demo exec demo.org bash "echo bye"

  # Use a named block with header arguments
  org-demo exec --name fetch-data --header-args ":session *py* :results output" \\
      demo.org python "import requests; print(requests.get('https://example.com').status_code)"

  # Call a named block with arguments
  org-demo call demo.org fetch-data

  # Verify all outputs still match
  org-demo verify demo.org

  # Extract commands to recreate the document
  org-demo extract demo.org

\b
Org-babel header arguments:
  The --header-args flag accepts any valid org-babel header arguments:
    :session NAME    Share interpreter state across blocks
    :dir PATH        Set working directory for execution
    :var NAME=VALUE  Pass variables to the code block
    :results TYPE    Control result format (output, value, table, etc.)
    :cache yes       Cache results (skip re-evaluation when unchanged)
    :tangle FILE     Extract code to file via org-babel-tangle
    :noweb yes       Enable <<reference>> syntax for literate programming
    :exports TYPE    Control what is exported (code, results, both, none)

\b
Environment variables:
  EMACS_SERVER   Name of the Emacs daemon (same as --daemon)
"""


def _make_document(workdir: str | None, daemon: str | None) -> OrgDocument:
    handler = EmacsHandler(daemon=daemon)
    return OrgDocument(handler=handler, workdir=workdir)


@click.group(help=_MAIN_HELP)
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
    """Create a new demo document.

    \b
    Examples:
      org-demo init demo.org "My Demo"
      org-demo init tutorial.org "Getting Started with Python"
    """
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
    """Append commentary (text or stdin).

    \b
    Examples:
      org-demo note demo.org "This demonstrates basic shell usage."
      echo "Multi-line note from stdin" | org-demo note demo.org
    """
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
@click.option(
    "--name",
    "-n",
    default=None,
    help="Name the block (#+NAME:) so it can be referenced by other blocks or #+CALL:.",
)
@click.option(
    "--header-args",
    "-H",
    default=None,
    help='Org-babel header arguments (e.g. ":session *py* :results output").',
)
@click.pass_context
def exec_cmd(
    ctx: click.Context,
    file: str,
    lang: str,
    code: str | None,
    name: str | None,
    header_args: str | None,
) -> None:
    """Run code, capture output, and append both to the document.

    The code block is inserted into the .org file and executed via
    org-babel-execute-src-block.  Babel handles language routing,
    session management, and result insertion natively.

    \b
    Examples:
      # Simple execution
      org-demo exec demo.org bash "echo hello"

      # Named block (can be referenced later)
      org-demo exec --name greet demo.org python "print('hello')"

      # With session (shared interpreter state)
      org-demo exec -H ":session *py*" demo.org python "x = 42"
      org-demo exec -H ":session *py*" demo.org python "print(x)"

      # Multiple header args
      org-demo exec -H ":session *R* :results output :dir /tmp" demo.org R "getwd()"

      # With variable passing
      org-demo exec -H ":var name=\\"world\\"" demo.org python "return f'hello {name}'"

      # Cache results (skip re-evaluation when code is unchanged)
      org-demo exec -H ":cache yes" demo.org python "2 + 2"

      # Read code from stdin
      cat script.py | org-demo exec demo.org python
    """
    if code is None:
        code = click.get_text_stream("stdin").read()
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        output = doc.exec(file, lang, code, name=name, header_args=header_args)
        # Print captured output to stdout (like showboat)
        if output:
            click.echo(output, nl=False)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# call
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.argument("name")
@click.argument("arguments", required=False)
@click.option(
    "--inside-header",
    "-I",
    default=None,
    help='Header args inside the call brackets (e.g. ":session *py*").',
)
@click.option(
    "--end-header",
    "-E",
    default=None,
    help='Header args after the call (e.g. ":results output").',
)
@click.pass_context
def call(
    ctx: click.Context,
    file: str,
    name: str,
    arguments: str | None,
    inside_header: str | None,
    end_header: str | None,
) -> None:
    """Call a named source block via #+CALL: syntax.

    This appends a #+CALL: line that invokes a previously defined named
    block.  Org-babel handles execution and result capture.

    \b
    The full #+CALL: syntax is:
      #+CALL: name[inside-header](arguments) end-header

    \b
    Examples:
      # Call a named block
      org-demo call demo.org greet

      # Call with arguments
      org-demo call demo.org greet "x=5, y=10"

      # Call with header overrides
      org-demo call -E ":results silent" demo.org greet

      # Call with inside-header args (affect the called block)
      org-demo call -I ":session *py*" demo.org process-data
    """
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        output = doc.call(
            file,
            name,
            arguments=arguments,
            inside_header=inside_header,
            end_header=end_header,
        )
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

    \b
    Examples:
      org-demo image demo.org screenshot.png
      org-demo image demo.org "![Architecture diagram](arch.png)"
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
    """Remove the most recent entry from the document.

    \b
    Examples:
      org-demo pop demo.org
    """
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
    """Re-run all code blocks and compare outputs.

    Reads the document, re-executes every source block via
    org-babel-execute-buffer, then compares old vs new results.
    Exits with code 1 if any outputs differ.

    \b
    Examples:
      org-demo verify demo.org
      org-demo verify demo.org --output verified.org
    """
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
    """Emit the commands that would recreate the document.

    Parses the .org file and outputs a sequence of org-demo commands
    (init, note, exec, call, image) that would reproduce it.

    \b
    Examples:
      org-demo extract demo.org
      org-demo extract demo.org --filename copy.org
      org-demo extract demo.org | bash   # re-run the demo
    """
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        commands = doc.extract(file, filename=filename)
        click.echo(commands)
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)


# ------------------------------------------------------------------
# tangle
# ------------------------------------------------------------------


@cli.command()
@click.argument("file")
@click.option(
    "--target-dir",
    "-d",
    default=None,
    type=click.Path(file_okay=False),
    help="Directory to write tangled files into.",
)
@click.pass_context
def tangle(ctx: click.Context, file: str, target_dir: str | None) -> None:
    """Extract source code via org-babel-tangle.

    Blocks with :tangle header arguments will have their code written
    to the specified files.  This is org-babel's native literate
    programming extraction mechanism.

    \b
    Examples:
      # Add a block with :tangle header, then extract it
      org-demo exec -H ":tangle hello.sh :shebang #!/bin/bash" demo.org bash "echo hello"
      org-demo tangle demo.org

      # Tangle into a specific directory
      org-demo tangle demo.org --target-dir ./src
    """
    doc = _make_document(ctx.obj["workdir"], ctx.obj["daemon"])
    try:
        files = doc.tangle(file, target_dir=target_dir)
        if files:
            for f in files:
                click.echo(f"Tangled: {f}")
        else:
            click.echo("No blocks with :tangle headers found.")
    except OrgDemoError as e:
        click.echo(str(e), err=True)
        raise SystemExit(1)
