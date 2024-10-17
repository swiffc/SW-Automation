using System; // Provides fundamental classes and base classes.
using System.Globalization; // Provides classes for culture-related information, such as language and calendars.
using System.IO; // Provides methods for working with files and directories.
using System.Windows.Data; // Provides interfaces and classes for data binding.

namespace SWinstaller
{
    // This class converts a file path into just the file name for use in WPF data binding scenarios.
    public class PathToFileNameConverter : IValueConverter
    {
        // The Convert method is called when data is being passed from the source to the UI.
        // It takes a file path as input and returns just the file name.
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            // Check if the input value is not null and is a string type (i.e., a file path).
            if (value != null && value is string path)
            {
                // Use the Path.GetFileName() method to extract the file name from the full path.
                return Path.GetFileName(path);
            }

            // Return null if the input value is null or not a string.
            return null;
        }

        // The ConvertBack method is used when data flows from the UI back to the source.
        // Not implemented here because converting from a file name back to a full path is not needed.
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            // Throw an exception to indicate that this method is not implemented.
            throw new NotImplementedException();
        }
    }
}
