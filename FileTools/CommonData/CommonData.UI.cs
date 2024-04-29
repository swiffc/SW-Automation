using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // User interface
        public static bool ToggleCreateDrawing { get; set; } = false;
        public static bool ToggleSave { get; set; } = false;
        public static bool ToggleDeleteFiles { get; set; } = false;



        public static void UI_DoubleChanged(string textBoxText, Action<double> setProperty)
        {
            if (double.TryParse(textBoxText, out double value))
            {
                setProperty(value);
                Properties.Settings.Default.Save();
            }
        }
        public static void UI_IntChanged(string textBoxText, Action<int> setProperty)
        {
            if (int.TryParse(textBoxText, out int value))
            {
                setProperty(value);
                Properties.Settings.Default.Save();
            }
        }
        public static void UI_BoolChanged(bool isChecked, Action<bool> setProperty)
        {
            setProperty(isChecked);
            Properties.Settings.Default.Save();
        }
        public static void UI_StringChanged(string textBoxText, Action<string> setProperty)
        {
            setProperty(textBoxText);
            Properties.Settings.Default.Save();
        }


    }
}
