
using System;
using System.Runtime.InteropServices;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Manages connection to SolidWorks COM API with robust error handling
    /// </summary>
    public class SolidWorksConnectionManager : ISolidWorksConnectionManager
    {
        private SldWorks _swApp;
        private bool _isConnected;
        private bool _disposed;

        public bool IsConnected => _isConnected;
        public SldWorks SwApp => _swApp;
        public string Version => _swApp?.RevisionNumber() ?? "Unknown";

        public event EventHandler<ConnectionStateChangedEventArgs> ConnectionStateChanged;
        public event EventHandler<SolidWorksErrorEventArgs> SolidWorksError;

        public SolidWorksConnectionManager()
        {
            GlobalErrorHandler.LogInfo("SolidWorksConnectionManager initialized");
        }

        public bool Connect(bool startIfNotRunning = true)
        {
            if (_isConnected)
            {
                GlobalErrorHandler.LogInfo("Already connected to SolidWorks");
                return true;
            }

            try
            {
                GlobalErrorHandler.LogInfo("Attempting to connect to SolidWorks...");

                // Try to get running instance first
                _swApp = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
                
                if (_swApp == null && startIfNotRunning)
                {
                    GlobalErrorHandler.LogInfo("No running instance found, starting SolidWorks...");
                    _swApp = (SldWorks)Activator.CreateInstance(Type.GetTypeFromProgID("SldWorks.Application"));
                }

                if (_swApp == null)
                {
                    throw new InvalidOperationException("Failed to connect to or start SolidWorks");
                }

                // Make visible
                _swApp.Visible = true;
                _isConnected = true;

                GlobalErrorHandler.LogInfo($"Connected to SolidWorks {Version}");
                OnConnectionStateChanged(true, $"Connected to SolidWorks {Version}");

                return true;
            }
            catch (COMException comEx)
            {
                var errorMsg = $"COM Error connecting to SolidWorks: {comEx.Message}";
                GlobalErrorHandler.LogError(comEx, errorMsg);
                OnSolidWorksError(errorMsg, comEx.ErrorCode, comEx);
                return false;
            }
            catch (Exception ex)
            {
                var errorMsg = $"Error connecting to SolidWorks: {ex.Message}";
                GlobalErrorHandler.LogError(ex, errorMsg);
                OnSolidWorksError(errorMsg, 0, ex);
                return false;
            }
        }

        public void Disconnect()
        {
            if (!_isConnected)
            {
                return;
            }

            try
            {
                GlobalErrorHandler.LogInfo("Disconnecting from SolidWorks...");
                
                if (_swApp != null)
                {
                    // Don't close SolidWorks, just release the reference
                    Marshal.ReleaseComObject(_swApp);
                    _swApp = null;
                }

                _isConnected = false;
                OnConnectionStateChanged(false, "Disconnected from SolidWorks");
                GlobalErrorHandler.LogInfo("Disconnected from SolidWorks");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error disconnecting from SolidWorks");
                OnSolidWorksError($"Error disconnecting: {ex.Message}", 0, ex);
            }
        }

        protected virtual void OnConnectionStateChanged(bool isConnected, string message)
        {
            ConnectionStateChanged?.Invoke(this, new ConnectionStateChangedEventArgs 
            { 
                IsConnected = isConnected, 
                Message = message 
            });
        }

        protected virtual void OnSolidWorksError(string message, int errorCode, Exception exception)
        {
            SolidWorksError?.Invoke(this, new SolidWorksErrorEventArgs 
            { 
                Message = message, 
                ErrorCode = errorCode, 
                Exception = exception 
            });
        }

        public void Dispose()
        {
            if (_disposed)
                return;

            Disconnect();
            _disposed = true;
            GC.SuppressFinalize(this);
        }

        ~SolidWorksConnectionManager()
        {
            Dispose();
        }
    }
}
