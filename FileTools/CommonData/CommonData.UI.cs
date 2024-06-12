using FileTools.Properties;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
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
            if (double.TryParse(textBoxText, out double value))
                setProperty(value);
            SaveSettings();
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
            try
            {
                setProperty(textBoxText[0]);
                SaveSettings();
            }
            catch (IndexOutOfRangeException ex)
            {
                Debug.WriteLine(ex.Message);
            }
        }
    }
}
