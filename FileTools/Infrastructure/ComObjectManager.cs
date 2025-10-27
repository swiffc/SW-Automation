using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace FileTools.Infrastructure
{
    /// <summary>
    /// Manages COM objects to ensure proper cleanup and prevent memory leaks
  /// Thread-safe implementation with automatic disposal
    /// </summary>
    public class ComObjectManager : IDisposable
    {
    private readonly List<object> _comObjects = new List<object>();
        private readonly object _lock = new object();
        private bool _disposed = false;

      /// <summary>
        /// Track a COM object for automatic cleanup
        /// </summary>
        /// <typeparam name="T">Type of COM object</typeparam>
        /// <param name="comObject">The COM object to track</param>
        /// <returns>The same COM object (for fluent usage)</returns>
        public T Track<T>(T comObject) where T : class
      {
          if (comObject == null)
        return null;

            lock (_lock)
        {
                if (!_disposed && Marshal.IsComObject(comObject))
       {
          _comObjects.Add(comObject);
    Debug.WriteLine($"[COM] Tracking: {typeof(T).Name} (Total: {_comObjects.Count})");
    }
            }
  return comObject;
        }

        /// <summary>
        /// Release a specific COM object immediately
        /// </summary>
        /// <typeparam name="T">Type of COM object</typeparam>
        /// <param name="comObject">Reference to the COM object (will be set to null)</param>
     public void Release<T>(ref T comObject) where T : class
        {
        if (comObject == null)
       return;

            lock (_lock)
            {
  try
 {
        if (Marshal.IsComObject(comObject))
  {
    int refCount = Marshal.ReleaseComObject(comObject);
       _comObjects.Remove(comObject);
         Debug.WriteLine($"[COM] Released: {typeof(T).Name}, RefCount: {refCount} (Remaining: {_comObjects.Count})");
  }
        }
      catch (Exception ex)
     {
Debug.WriteLine($"[COM] Error releasing {typeof(T).Name}: {ex.Message}");
    }
        finally
   {
          comObject = null;
      }
        }
        }

        /// <summary>
    /// Release all tracked COM objects
        /// </summary>
        public void ReleaseAll()
        {
lock (_lock)
         {
            int count = _comObjects.Count;
        Debug.WriteLine($"[COM] Releasing {count} COM objects...");
  
    for (int i = _comObjects.Count - 1; i >= 0; i--)
   {
            var comObject = _comObjects[i];
   if (comObject != null)
  {
    try
             {
      if (Marshal.IsComObject(comObject))
  {
        int refCount = Marshal.FinalReleaseComObject(comObject);
  Debug.WriteLine($"[COM] Released object at index {i}, Final RefCount: {refCount}");
   }
      }
    catch (Exception ex)
    {
        Debug.WriteLine($"[COM] Error releasing object at index {i}: {ex.Message}");
  }
                    }
                }
      
_comObjects.Clear();
           Debug.WriteLine($"[COM] All COM objects released");
            }
        }

        /// <summary>
        /// Gets the count of currently tracked COM objects
        /// </summary>
   public int TrackedObjectCount
        {
            get
    {
 lock (_lock)
      {
     return _comObjects.Count;
          }
      }
        }

        public void Dispose()
      {
      Dispose(true);
    GC.SuppressFinalize(this);
 }

        protected virtual void Dispose(bool disposing)
        {
            if (!_disposed)
            {
       if (disposing)
        {
           ReleaseAll();
       }
        _disposed = true;
     }
        }

        ~ComObjectManager()
        {
   Dispose(false);
        }
    }
}
