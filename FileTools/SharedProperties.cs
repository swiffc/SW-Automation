namespace FileTools
{
    public static class SharedProperties
    {
<<<<<<< HEAD
        // Footprint
        static public double Width { get; set; } = 108;
        static public double Length { get; set; } = 240;
        static public double ColumnHeight { get; set; } = 120;
=======
        // Structure
        static public double Width { get; set; } = 108;
        static public double Length { get; set; } = 360;
        static public double TotalColumnHeight { get; set; } = 120;
>>>>>>> releases/v4.0.0
        static private bool _midColumns = true;
        #region MidColumn rules

        static public bool MidColumns
        {
            get => FanCount != 1 && _midColumns;
            set => MidColumnsInternal = value;
        }
        static private bool MidColumnsInternal
        {
            get => _midColumns;
            set => _midColumns = value;
        }

        #endregion
<<<<<<< HEAD


        // Plenum
        private static int _fanCount = 2;
=======
        static public double ClipHeight { get; set; } = 20;
        private static string _braceType = "L";
        #region BraceType Rules

        public static string BraceType
        {
            get
            {
                return _braceType;
            }
            set
            {
                if (value == "L" || value == "LL" || value == "T" || value == "X" || value == "TX")
                    _braceType = value;
                else
                    _braceType = "L";
            }
        }

        #endregion
        static public double BraceAngle { get; set; } = 30;


        // Plenum
        private static int _fanCount = 3;
>>>>>>> releases/v4.0.0
        #region FanCount rules

        static public int FanCount
        {
            get => _fanCount;
            set
            {
                if (value != _fanCount)
                {
                    _fanCount = value >= 1 ? value : 1;
<<<<<<< HEAD
                    OnFanCountChanged?.Invoke(); 
=======
                    OnFanCountChanged?.Invoke();
>>>>>>> releases/v4.0.0
                }
            }
        }
        public delegate void PropertyChangeHandler();
        public static event PropertyChangeHandler OnFanCountChanged;

        #endregion
        static public double PlenumDepth { get; set; } = 36;
<<<<<<< HEAD


        // Machinery Mount
        static public double MachineryMountHeight { get; set; } = 24;


        // Shipping steel
        static public double ShippingBeamHeight { get; set; } = 6;
=======
        static public double PlenumClipHole { get; set; } = 2.5;


        // MachineryMount
        static public double MachineryMountHeight { get; set; } = 24;


        // ShippingBeam
        static public double ShippingBeamHeight { get; set; } = 10;


        // Framework
        public static bool ToggleCreateDrawing { get; set; } = true;
        public static bool ToggleSave { get; set; } = true;
        public static bool ToggleDeleteFiles { get; set; } = true;
>>>>>>> releases/v4.0.0
    }
}
