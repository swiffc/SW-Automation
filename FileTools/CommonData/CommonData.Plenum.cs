using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Base.Part;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // Plenum
        private static int _fanCount = 2;
        static public int Fan_Count
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
        static public double Plenum_Depth { get; set; } = 36;
        static public double BottomOfPlenumToClipHole { get; set; } = 2.5;
        static public double EndPanel_THK => Default.EndPanel_THK;
        static public double SidePanel_THK => Default.SidePanel_THK;
        public static Design PlenumDesign
        {
            get
            {
                if (Enum.TryParse(Default.PlenumDesignSetting, out Design design))
                {
                    return design;
                }
                return Design.Standard; 
            }
            set
            {
                Default.PlenumDesignSetting = value.ToString();
                Default.Save(); 
            }
        }
        public static Spec MaterialSpec
        {
            get
            {
                if (Enum.TryParse(Default.MaterialSpecSetting, out Spec spec))
                {
                    return spec;
                }
                return Spec.A36;
            }
            set
            {
                Default.MaterialSpecSetting = value.ToString();
                Default.Save();
            }
        }





        public enum Design
        {
            Standard,
            Johnson,
            Legacy
        }
        public static double _fanDiameterFeet => Default.Fan_Diameter_Feet;
        public static double FanDiameter
        {
            get
            {
                return _fanDiameterFeet * 12;
            }
            set
            {
                Default.Fan_Diameter_Feet = value;
            }
        }
        private static double _ringDepth => Default.FanRing_Depth;
        public static double Ring_Depth
        {
            get
            {
                var roundedUp = Math.Ceiling(_ringDepth);
                return roundedUp % 2 == 0 ? roundedUp : roundedUp + 1;
            }
            set
            {
                Default.FanRing_Depth = value;
            }
        }
    }
}
