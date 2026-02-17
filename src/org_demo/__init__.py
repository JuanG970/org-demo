"""org-demo — executable demo documents backed by org-mode."""

__version__ = "0.1.0"

from .emacs_handler import EmacsHandler, EmacsResponse
from .document import OrgDocument, OrgDemoError

__all__ = [
    "EmacsHandler",
    "EmacsResponse",
    "OrgDocument",
    "OrgDemoError",
]
