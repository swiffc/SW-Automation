
using System;
using SolidWorks.Interop.sldworks;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Interface for managing SolidWorks COM API connection
    /// </summary>
    public interface ISolidWorksConnectionManager : IDisposable
    {
        /// <summary>
        /// Gets whether SolidWorks is currently connected
        /// </summary>
        bool IsConnected { get; }

        /// <summary>
        /// Gets the current SolidWorks application instance
        /// </summary>
        SldWorks SwApp { get; }

        /// <summary>
        /// Gets the current SolidWorks version
        /// </summary>
        string Version { get; }

        /// <summary>
        /// Connects to SolidWorks. Creates new instance if not running, or attaches to existing.
        /// </summary>
        /// <param name="startIfNotRunning">If true, starts SolidWorks if not already running</param>
        /// <returns>True if connection successful</returns>
        bool Connect(bool startIfNotRunning = true);

        /// <summary>
        /// Disconnects from SolidWorks
        /// </summary>
        void Disconnect();

        /// <summary>
        /// Event raised when connection state changes
        /// </summary>
        event EventHandler<ConnectionStateChangedEventArgs> ConnectionStateChanged;

        /// <summary>
        /// Event raised when SolidWorks encounters an error
        /// </summary>
        event EventHandler<SolidWorksErrorEventArgs> SolidWorksError;
    }

    /// <summary>
    /// Connection state changed event args
    /// </summary>
    public class ConnectionStateChangedEventArgs : EventArgs
    {
        public bool IsConnected { get; set; }
        public string Message { get; set; }
    }

    /// <summary>
    /// SolidWorks error event args
    /// </summary>
    public class SolidWorksErrorEventArgs : EventArgs
    {
        public string Message { get; set; }
        public int ErrorCode { get; set; }
        public Exception Exception { get; set; }
    }
}
