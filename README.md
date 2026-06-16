# org-demo

Create executable demo documents backed by org-mode.

org-demo helps agents build Emacs org-mode documents that mix commentary,
executable code blocks, and captured output. These documents serve as both
readable documentation and reproducible proof of work.

## Prerequisites

- **Python 3.10+**
- **[uv](https://docs.astral.sh/uv/)** — Python package manager
- **Emacs** with an org-mode capable daemon running (e.g. `emacs --daemon`)

## Install

```bash
# Clone and enter the project
git clone <repo-url> org-demo
cd org-demo

# Install with uv (creates a venv and installs the package in editable mode)
uv sync

# Or install globally via pipx after building
make install
```

## Run

```bash
# Activate the virtual environment
source .venv/bin/activate

# Or use uv to run commands without activating
uv run org-demo --help

# Create a new demo document
uv run org-demo init demo.org "My Demo"

# Add commentary
uv run org-demo note demo.org "This demonstrates basic shell usage."

# Execute code and capture output
uv run org-demo exec demo.org bash "echo hello"

# Verify all outputs still match
uv run org-demo verify demo.org

# Extract commands to recreate the document
uv run org-demo extract demo.org
```

## Development

```bash
# Sync dependencies (including dev)
uv sync

# Run tests
uv run pytest

# Build distribution
uv build
```
