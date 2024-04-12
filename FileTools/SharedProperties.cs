namespace FileTools
{
    public static class SharedProperties
    {
        // Footprint
        static public double Width { get; set; } = 108;
        static public double Length { get; set; } = 240;
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


        // Plenum
        private static int _fanCount = 2;
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


        // Machinery Mount
        static public double MachineryMountHeight { get; set; } = 24;


        // Shipping steel
        static public double ShippingBeamHeight { get; set; } = 6;
    }
}
