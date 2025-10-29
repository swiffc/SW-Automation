
using System;
using SolidWorks.Interop.sldworks;
using UnifiedUI.Services.SolidWorksApi;

namespace SolidWorksAutomation.Tests.Mocks
{
    /// <summary>
    /// Mock implementation of ISolidWorksConnectionManager for testing without SolidWorks
    /// </summary>
    public class MockSolidWorksConnectionManager : ISolidWorksConnectionManager
    {
        private bool _isConnected;
        private bool _shouldFailConnection;

        public bool IsConnected => _isConnected;
        public SldWorks SwApp => null; // Return null for mock
        public string Version => "MockVersion 2024";

        public event EventHandler<ConnectionStateChangedEventArgs> ConnectionStateChanged;
        public event EventHandler<SolidWorksErrorEventArgs> SolidWorksError;

        public MockSolidWorksConnectionManager(bool shouldFailConnection = false)
        {
            _shouldFailConnection = shouldFailConnection;
        }

        public bool Connect(bool startIfNotRunning = true)
        {
            if (_shouldFailConnection)
            {
                OnSolidWorksError("Mock connection failed", 0, new Exception("Simulated connection failure"));
                return false;
            }

            _isConnected = true;
            OnConnectionStateChanged(true, "Mock connected successfully");
            return true;
        }

        public void Disconnect()
        {
            _isConnected = false;
            OnConnectionStateChanged(false, "Mock disconnected");
        }

        public void Dispose()
        {
            Disconnect();
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
    }
}
