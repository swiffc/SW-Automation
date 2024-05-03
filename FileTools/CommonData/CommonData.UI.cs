using FileTools.Properties;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        public static event Action SettingsChanged;
        public static void SaveSettings()
        {
            Default.Save();
            SettingsChanged?.Invoke();
        }

        public static void UI_DoubleChanged(string textBoxText, Action<double> setProperty)
        {
            bool saveIsEnabled = true;
            if (
                (textBoxText.StartsWith("-0") && textBoxText.EndsWith(".0")) || // -0.0
                (textBoxText.StartsWith("-0") && textBoxText.EndsWith(".")) ||  // -0.
                (textBoxText.StartsWith("-") && textBoxText.EndsWith("0")) ||   // -0
                (textBoxText.StartsWith("-") && textBoxText.EndsWith("-")) ||   // -
                (textBoxText.StartsWith("-") && textBoxText.EndsWith(".")) ||   // - .
                (textBoxText.StartsWith("-") && textBoxText.EndsWith(".0")) ||  // - .0
                (textBoxText.StartsWith("0") && textBoxText.EndsWith(".0")) ||  //  0.0
                (textBoxText.StartsWith("0") && textBoxText.EndsWith(".")) ||   //  0.
                (textBoxText.StartsWith(".") && textBoxText.EndsWith(".")) ||   //   .
                (textBoxText.StartsWith(".") && textBoxText.EndsWith(".0")))    //   .0
            {
                saveIsEnabled = false;
            }
            else if (double.TryParse(textBoxText, out double value))
            {
                setProperty(value);
            }
            else
            {
                setProperty(0.001);
            }

            if (saveIsEnabled)
            {
                SaveSettings();
            }
                
                
        }
        public static void UI_IntChanged(string textBoxText, Action<int> setProperty)
        {
            if (int.TryParse(textBoxText, out int value))
                setProperty(value);
            SaveSettings();
        }
        public static void UI_BoolChanged(bool isChecked, Action<bool> setProperty)
        {
            setProperty(isChecked);
            SaveSettings();
        }
        public static void UI_StringChanged(string textBoxText, Action<string> setProperty)
        {
            setProperty(textBoxText);
            SaveSettings();
        }
        public static void UI_CharChanged(string textBoxText, Action<char> setProperty)
        {
            setProperty(textBoxText[0]);
            SaveSettings();
        }
    }
}
