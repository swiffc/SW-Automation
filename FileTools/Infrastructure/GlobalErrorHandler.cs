using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace FileTools.Infrastructure
{
/// <summary>
/// Global error handler for all applications
    /// Handles all unhandled exceptions and provides logging
    /// </summary>
    public static class GlobalErrorHandler
    {
        private static string _logFilePath;
        private static readonly object _logLock = new object();
        private static bool _isInitialized = false;

   /// <summary>
  /// Initialize the global error handler - MUST be called first in Program.Main
   /// </summary>
        public static void Initialize()
  {
            if (_isInitialized)
     return;

 // Set up log file path
   string logDirectory = Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
          "BundleApp",
            "Logs");
          
            Directory.CreateDirectory(logDirectory);
     _logFilePath = Path.Combine(logDirectory, $"BundleApp_{DateTime.Now:yyyyMMdd_HHmmss}.log");

  // Subscribe to error events
            Application.ThreadException += OnThreadException;
            AppDomain.CurrentDomain.UnhandledException += OnUnhandledException;
   Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);

   _isInitialized = true;
            LogInfo("=== Application Started ===");
     LogInfo($"Log file: {_logFilePath}");
  }

        private static void OnThreadException(object sender, ThreadExceptionEventArgs e)
        {
  HandleException(e.Exception, "UI Thread Exception");
     }

        private static void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
  var exception = e.ExceptionObject as Exception;
            HandleException(exception, "Unhandled Exception", e.IsTerminating);
        }

        private static void HandleException(Exception ex, string source, bool isTerminating = false)
        {
 try
       {
            // Log the error
     LogError(ex, source);

          // Show user-friendly message
           ShowUserFriendlyError(ex, isTerminating);
   }
   catch (Exception loggingEx)
            {
            // Last resort - show basic message
MessageBox.Show(
$"A critical error occurred:\n{ex?.Message}\n\nAdditional error while logging: {loggingEx.Message}",
     "Critical Error",
        MessageBoxButtons.OK,
       MessageBoxIcon.Error);
            }
        }

        public static void LogError(Exception ex, string context = "")
        {
            var sb = new StringBuilder();
    sb.AppendLine($"[{DateTime.Now:yyyy-MM-dd HH:mm:ss.fff}] ERROR");
    if (!string.IsNullOrEmpty(context))
       sb.AppendLine($"Context: {context}");
     
  sb.AppendLine($"Message: {ex?.Message}");
            sb.AppendLine($"Type: {ex?.GetType().FullName}");
            
if (ex is COMException comEx)
          {
                sb.AppendLine($"COM HRESULT: 0x{comEx.HResult:X8}");
     }
   
            sb.AppendLine($"Stack Trace:\n{ex?.StackTrace}");

      if (ex?.InnerException != null)
         {
   sb.AppendLine("\nInner Exception:");
sb.AppendLine($"  Message: {ex.InnerException.Message}");
      sb.AppendLine($"  Type: {ex.InnerException.GetType().FullName}");
         sb.AppendLine($"  Stack Trace:\n  {ex.InnerException.StackTrace}");
     }

            sb.AppendLine(new string('=', 100));

          WriteToLog(sb.ToString());
            Debug.WriteLine(sb.ToString());
      }

public static void LogInfo(string message)
        {
     var logMessage = $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss.fff}] INFO: {message}";
  WriteToLog(logMessage);
     Debug.WriteLine(logMessage);
 }

        public static void LogWarning(string message)
        {
          var logMessage = $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss.fff}] WARNING: {message}";
       WriteToLog(logMessage);
  Debug.WriteLine(logMessage);
        }

        public static void LogDebug(string message)
        {
      #if DEBUG
            var logMessage = $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss.fff}] DEBUG: {message}";
  WriteToLog(logMessage);
            Debug.WriteLine(logMessage);
    #endif
        }

        private static void WriteToLog(string message)
 {
        lock (_logLock)
          {
       try
       {
     File.AppendAllText(_logFilePath, message + Environment.NewLine);
        }
     catch
        {
        // If we can't write to log, at least show in debug
    Debug.WriteLine($"Failed to write to log: {message}");
         }
   }
        }

        private static void ShowUserFriendlyError(Exception ex, bool isTerminating)
        {
string title;
            string message;
        MessageBoxIcon icon;

            // Categorize the exception
        if (ex is InvalidOperationException && ex.Message.Contains("SolidWorks"))
            {
                title = "SolidWorks Connection Error";
         message = ex.Message;
                icon = MessageBoxIcon.Warning;
}
            else if (ex is COMException comEx)
   {
                title = "COM Interop Error";
     message = GetCOMErrorMessage(comEx);
       icon = MessageBoxIcon.Error;
       }
      else if (ex is TypeInitializationException typeEx)
            {
  title = "Initialization Error";
           message = $"Failed to initialize a required component.\n\n" +
   $"Component: {typeEx.TypeName}\n" +
       $"Reason: {typeEx.InnerException?.Message ?? typeEx.Message}\n\n" +
              $"Please restart the application.";
         icon = MessageBoxIcon.Error;
   }
else if (ex is FileNotFoundException || ex is DirectoryNotFoundException)
            {
      title = "File Not Found";
          message = $"A required file or directory was not found:\n\n{ex.Message}\n\n" +
    $"Please check that all required files are in place.";
      icon = MessageBoxIcon.Warning;
        }
    else if (ex is UnauthorizedAccessException)
   {
      title = "Access Denied";
       message = $"The application does not have permission to access a required resource:\n\n{ex.Message}\n\n" +
     $"Try running as administrator.";
 icon = MessageBoxIcon.Error;
            }
    else
 {
         title = "Application Error";
             message = $"An unexpected error occurred:\n\n{ex?.Message}\n\n" +
        $"Error Type: {ex?.GetType().Name}";
                icon = MessageBoxIcon.Error;
       }

  if (isTerminating)
          {
     message += "\n\nThe application will now close.";
            }
  else
       {
    message += "\n\nYou may continue working, but some features may not work correctly.";
      }

            message += $"\n\nError details have been logged to:\n{_logFilePath}";

     MessageBox.Show(message, title, MessageBoxButtons.OK, icon);
        }

        private static string GetCOMErrorMessage(COMException comEx)
   {
   string specificMessage = "";
            
            switch ((uint)comEx.HResult)
            {
  case 0x800401E3: // MK_E_UNAVAILABLE
          specificMessage = "The COM object is not available. This usually means:\n" +
           "• SolidWorks or Excel is not running\n" +
       "• The application has been closed\n" +
    "• The COM server is not registered";
      break;
         case 0x80040154: // REGDB_E_CLASSNOTREG
      specificMessage = "The COM class is not registered. Please reinstall the application.";
   break;
     case 0x80010108: // RPC_E_DISCONNECTED
            specificMessage = "The connection to the COM server was lost.";
 break;
      default:
           specificMessage = $"A COM error occurred (0x{comEx.HResult:X8})";
        break;
    }

  return $"{specificMessage}\n\nDetails: {comEx.Message}";
        }

 /// <summary>
    /// Opens the log file in the default text editor
        /// </summary>
   public static void OpenLogFile()
        {
    try
  {
                if (File.Exists(_logFilePath))
   {
        Process.Start(_logFilePath);
    }
             else
                {
     MessageBox.Show("No log file exists yet.", "No Log File", MessageBoxButtons.OK, MessageBoxIcon.Information);
   }
         }
 catch (Exception ex)
 {
       MessageBox.Show($"Failed to open log file:\n{ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// Opens the log directory
        /// </summary>
        public static void OpenLogDirectory()
        {
          try
 {
      string directory = Path.GetDirectoryName(_logFilePath);
        if (Directory.Exists(directory))
                {
Process.Start(directory);
  }
}
   catch (Exception ex)
       {
   MessageBox.Show($"Failed to open log directory:\n{ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
    }
        }

      /// <summary>
        /// Gets the current log file path
        /// </summary>
        public static string LogFilePath => _logFilePath;
    }
}
