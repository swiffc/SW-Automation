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
        // Plenum
        private static int _fanCount = 2;
        static public int FanCount
        #region FanCount rules
        {
            get => _fanCount;
            set
            {
                if (value != _fanCount)
                {
                    _fanCount = value >= 1 ? value : 1;
                    OnFanCountChanged?.Invoke();
                }
            }
        }
        public delegate void PropertyChangeHandler();
        public static event PropertyChangeHandler OnFanCountChanged;

        #endregion
        static public double PlenumDepth { get; set; } = 36;
        static public double BottomOfPlenumToClipHole { get; set; } = 2.5;
        static public double EndPanel_THK => Default.EndPanel_THK;
        static public double SidePanel_THK => Default.SidePanel_THK;
        // Property to get and set the PlenumDesign with conversion
        public static Design PlenumDesign
        {
            get
            {
                if (Enum.TryParse(Properties.Settings.Default.PlenumDesignSetting, out Design design))
                {
                    return design;
                }
                return Design.Standard; // Or your default
            }
            set
            {
                Properties.Settings.Default.PlenumDesignSetting = value.ToString();
                Properties.Settings.Default.Save(); // Don't forget to save the setting
            }
        }
        public enum Design
        {
            Standard,
            Johnson,
            Legacy
        }
    }
}
