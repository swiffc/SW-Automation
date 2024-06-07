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
        static public int Fan_Count
        #region FanCount rules
        {
            get => Default.Fan_Count;
            set
            {
                if (value != Default.Fan_Count)
                {
                    Default.Fan_Count = value >= 1 ? value : 1;
                    OnFanCountChanged?.Invoke();
                }
            }
        }
        public delegate void PropertyChangeHandler();
        public static event PropertyChangeHandler OnFanCountChanged;

        #endregion
        static public double Plenum_Depth => Default.Plenum_Depth;
        static public double BottomOfPlenumToClipHole { get; set; } = 2.5;
        static public double EndPanel_THK => Default.EndPanel_THK;
        static public double SidePanel_THK => Default.SidePanel_THK;
        public static Design Plenum_Design
        {
            get
            {
                if (Enum.TryParse(Default.PlenumDesignSetting, out Design design))
                {
                    return design;
                }
                throw new NotImplementedException();
            }
            set
            {
                Default.PlenumDesignSetting = value.ToString();
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
            }
        }
        public static double PlenumColumn_Height
        {
            get
            {
                var value = TotalColumnHeight - FieldColumn_Height;
                return value;
            }
        }

        public static double Johnson_ExtraLength
        {
            get
            {
                if (Default.Johnson_ExtraLength < MinimumExtraLength)
                {
                    return MinimumExtraLength;
                }
                return Default.Johnson_ExtraLength;
            }
            set
            {
                if (value < MinimumExtraLength)
                {
                    Default.Johnson_ExtraLength = MinimumExtraLength;
                }
                else
                {
                    Default.Johnson_ExtraLength = value;
                }
            }
        }
        private static double MinimumExtraLength = 12;

        public enum Design
        {
            Standard,
            Johnson,
            Legacy
        }
        public static double FanDiameter_Feet => Default.Fan_Diameter_Feet;
        public static double FanDiameter_Inches
        {
            get
            {
                return FanDiameter_Feet * 12;
            }
            set
            {
                Default.Fan_Diameter_Feet = value;
            }
        }
        public static double Ring_Depth
        {
            get
            {
                return Default.FanRing_Depth;
            }
            set
            {
                var roundedUp = Math.Ceiling(value);
                Default.FanRing_Depth = roundedUp < 8 ? 8 : (roundedUp % 2 == 0 ? roundedUp : roundedUp + 1);
            }
        }
        public static int EndStiffenerCount => Default.EndStiffenerCount;
        public static int DividerStiffenerCount => Default.DividerStiffenerCount;
        static public double FanRing_Depth
        {
            get
            {
                return Default.FanRing_Depth;
            }
            set
            {
                Default.FanRing_Depth = value;
            }
        }
    }
}
