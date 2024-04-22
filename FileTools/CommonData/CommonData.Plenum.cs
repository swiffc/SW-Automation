using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
        static public double EndPanel_THK { get; set; } = 0.1344;
        static public double SidePanel_THK { get; set; } = 0.1344;
        public static Design PlenumDesign { get; set; }
        public enum Design
        {
            Standard,
            Johnson,
            Legacy
        }
    }
}
