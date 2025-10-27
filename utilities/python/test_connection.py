"""
Test script to verify SolidWorks API connection via Python
Run this after installing Python packages to ensure everything is working
"""

import sys

def test_solidworks_connection():
    """Test connection to SolidWorks application"""
    
    print("=" * 60)
    print("SolidWorks API Connection Test")
    print("=" * 60)
    
    # Test 1: Import required modules
    print("\n[1/4] Testing module imports...")
    try:
        import win32com.client
        import pythoncom
        print("    ? win32com imported successfully")
    except ImportError as e:
        print(f"    ? Failed to import win32com: {e}")
        print("    ? Run: pip install pywin32")
        return False
    
    # Test 2: Connect to SolidWorks
    print("\n[2/4] Connecting to SolidWorks...")
    try:
        sw = win32com.client.Dispatch("SldWorks.Application")
        print("    ? Connected to SolidWorks")
    except Exception as e:
        print(f"    ? Failed to connect: {e}")
        print("    ? Make sure SolidWorks is installed and registered")
        return False
    
    # Test 3: Get SolidWorks version
    print("\n[3/4] Getting SolidWorks version...")
    try:
        version = sw.RevisionNumber()
        print(f"    ? SolidWorks Version: {version}")
    except Exception as e:
        print(f"    ? Failed to get version: {e}")
        return False
    
    # Test 4: Check for active document
    print("\n[4/4] Checking for active document...")
    try:
        model = sw.ActiveDoc
        if model:
            title = model.GetTitle()
            doc_type = model.GetType()
            type_names = {1: "Part", 2: "Assembly", 3: "Drawing"}
            type_name = type_names.get(doc_type, "Unknown")
            print(f"    ? Active document: {title} ({type_name})")
        else:
            print("    ? No active document (this is normal if no file is open)")
    except Exception as e:
        print(f"    ? Error checking document: {e}")
    
    # Success
    print("\n" + "=" * 60)
    print("? All tests passed! SolidWorks API is ready to use.")
    print("=" * 60)
    return True

if __name__ == "__main__":
    try:
        success = test_solidworks_connection()
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n\n? Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

