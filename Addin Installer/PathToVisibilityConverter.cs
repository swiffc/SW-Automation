using System; // Provides fundamental classes and base classes.
using System.Globalization; // Provides classes for culture-related information, such as language and calendars.
using System.Windows; // Provides classes for creating Windows-based applications, including UI elements.
using System.Windows.Data; // Provides interfaces and classes for data binding.

namespace SWinstaller
{
    // This attribute specifies the types this converter works with: string to Visibility.
    [ValueConversion(typeof(string), typeof(Visibility))]
    public class PathToVisibilityConverter : IValueConverter
    {
        // The Convert method is called when data is being passed from the source to the UI.
        // It converts a file path string to a Visibility value based on whether the path contains a specified parameter string.
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            // Check if the input value or parameter is null.
            if (value == null || parameter == null)
                return null;

            // Cast the input value to a string (expected to be a file path).
            var path = (string)value;
            // Cast the parameter to a string (expected to be a keyword or substring to look for).
            var parameterString = (string)parameter;

            // Check if the path contains the parameter string, ignoring case.
            // If true, return Visibility.Visible; otherwise, return Visibility.Collapsed.
            return path.ToLower().Contains(parameterString.ToLower()) ? Visibility.Visible : Visibility.Collapsed;
        }

        // The ConvertBack method is not implemented because converting back from Visibility to a string path is not needed.
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            => throw new NotImplementedException();
    }
}
