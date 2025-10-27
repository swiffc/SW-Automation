"""
SolidWorks Automation Helper Module
====================================
Provides utility functions for SolidWorks automation using Python

Usage:
    from solidworks_helper import SolidWorksApp
    
    with SolidWorksApp() as sw:
        model = sw.get_active_document()
        print(f"Working on: {model.GetTitle()}")
"""

import win32com.client
import pythoncom
from typing import Optional


class SolidWorksApp:
    """
    Context manager for SolidWorks application
    
    Example:
        with SolidWorksApp() as sw:
            model = sw.get_active_document()
    """
    
    def __init__(self, visible: bool = True):
        """
        Initialize SolidWorks application
        
        Args:
            visible: Make SolidWorks window visible (default: True)
        """
        self.visible = visible
        self.app = None
        
    def __enter__(self):
        """Connect to SolidWorks when entering context"""
        try:
            self.app = win32com.client.Dispatch("SldWorks.Application")
            self.app.Visible = self.visible
            return self
        except Exception as e:
            raise ConnectionError(f"Failed to connect to SolidWorks: {e}")
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Clean up when exiting context"""
        self.app = None
        return False
    
    def get_active_document(self):
        """
        Get the currently active document
        
        Returns:
            ModelDoc2 object or None if no document is active
        """
        if not self.app:
            raise RuntimeError("Not connected to SolidWorks")
        return self.app.ActiveDoc
    
    def open_document(self, filepath: str, doc_type: int = 1):
        """
        Open a SolidWorks document
        
        Args:
            filepath: Full path to the document
            doc_type: 1=Part, 2=Assembly, 3=Drawing
            
        Returns:
            ModelDoc2 object or None if failed
        """
        if not self.app:
            raise RuntimeError("Not connected to SolidWorks")
        
        errors = 0
        warnings = 0
        doc = self.app.OpenDoc6(filepath, doc_type, 0, "", errors, warnings)
        
        if errors:
            raise IOError(f"Failed to open document: Error code {errors}")
        
        return doc
    
    def save_document(self, model, filepath: Optional[str] = None):
        """
        Save a document
        
        Args:
            model: ModelDoc2 object to save
            filepath: Optional new filepath for Save As
        """
        if filepath:
            # Save As
            errors = 0
            warnings = 0
            success = model.SaveAs3(filepath, 0, 0)
            if not success:
                raise IOError(f"Failed to save document to {filepath}")
        else:
            # Regular save
            success = model.Save3(0, 0, 0)
            if not success:
                raise IOError("Failed to save document")
    
    def get_version(self) -> str:
        """Get SolidWorks version"""
        if not self.app:
            raise RuntimeError("Not connected to SolidWorks")
        return self.app.RevisionNumber()


def get_document_type_name(doc_type: int) -> str:
    """
    Convert document type code to readable name
    
    Args:
        doc_type: 1=Part, 2=Assembly, 3=Drawing
        
    Returns:
        String name of document type
    """
    types = {
        1: "Part",
        2: "Assembly",
        3: "Drawing"
    }
    return types.get(doc_type, "Unknown")


def main():
    """Example usage of SolidWorksApp"""
    
    print("SolidWorks Automation Example")
    print("=" * 50)
    
    with SolidWorksApp() as sw:
        print(f"? Connected to SolidWorks {sw.get_version()}")
        
        # Get active document
        model = sw.get_active_document()
        if model:
            title = model.GetTitle()
            doc_type = model.GetType()
            type_name = get_document_type_name(doc_type)
            print(f"? Active document: {title} ({type_name})")
        else:
            print("? No active document")
        
        print("\n" + "=" * 50)
        print("Done!")


if __name__ == "__main__":
    main()

