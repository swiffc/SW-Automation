namespace FileTools
{
    public static class SharedProperties
    {
        // Structure
        static public double Width { get; set; } = 108;
        static public double Length { get; set; } = 360;
        static public double ColumnHeight { get; set; } = 120;
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
                if (value == "L" || value == "LL" || value == "T" || value == "X")
                    _braceType = value;
                else
                    _braceType = "L";
            }
        }

        #endregion
        static public double BraceAngle { get; set; } = 30;


        // Plenum
        private static int _fanCount = 3;
        #region FanCount rules

        static public int FanCount
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


        // MachineryMount
        static public double MachineryMountHeight { get; set; } = 24;


        // ShippingBeam
        static public double ShippingBeamHeight { get; set; } = 10;
    }
}
