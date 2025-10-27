using System;
using System.Windows;
using FileTools.Infrastructure;

namespace UnifiedUI
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        protected override void OnStartup(StartupEventArgs e)
        {
            // Initialize GlobalErrorHandler first (before anything else)
            GlobalErrorHandler.Initialize();
            GlobalErrorHandler.LogInfo("=== UnifiedUI Application Started ===");
            GlobalErrorHandler.LogInfo($"OS: {Environment.OSVersion}");
            GlobalErrorHandler.LogInfo($".NET Version: {Environment.Version}");

            base.OnStartup(e);

            // Set up global exception handling with GlobalErrorHandler
            AppDomain.CurrentDomain.UnhandledException += OnUnhandledException;
            DispatcherUnhandledException += OnDispatcherUnhandledException;

            GlobalErrorHandler.LogInfo("UnifiedUI initialization complete");
        }

        protected override void OnExit(ExitEventArgs e)
        {
            GlobalErrorHandler.LogInfo("=== UnifiedUI Application Closing ===");
            base.OnExit(e);
        }

        private void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            var exception = e.ExceptionObject as Exception;

            // Log to file
            GlobalErrorHandler.LogError(exception, "Unhandled Exception (Critical)");

            // Show user-friendly message
            MessageBox.Show(
                $"A critical error occurred:\n\n{exception?.Message}\n\n" +
                $"Error details have been logged to:\n{GlobalErrorHandler.LogFilePath}\n\n" +
                $"The application will now close.",
                "Critical Error",
                MessageBoxButton.OK,
                MessageBoxImage.Error);
        }

        private void OnDispatcherUnhandledException(object sender, System.Windows.Threading.DispatcherUnhandledExceptionEventArgs e)
        {
            // Log to file
            GlobalErrorHandler.LogError(e.Exception, "Dispatcher Unhandled Exception");

            // Show user-friendly message
            MessageBox.Show(
                $"An error occurred:\n\n{e.Exception.Message}\n\n" +
                $"If this persists, check the log file:\n{GlobalErrorHandler.LogFilePath}",
                "Application Error",
                MessageBoxButton.OK,
                MessageBoxImage.Warning);

            e.Handled = true; // Prevent application crash
        }
    }
}
