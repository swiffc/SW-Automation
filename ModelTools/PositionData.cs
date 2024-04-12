namespace ModelTools
{
    public struct PositionData
    {
        public double TranslationX { get; set; }
        public double TranslationY { get; set; }
        public double TranslationZ { get; set; }
        public double RotationX { get; set; }
        public double RotationY { get; set; }
        public double RotationZ { get; set; }

        public PositionData(double tX, double tY, double tZ, double rX, double rY, double rZ)
        {
            TranslationX = tX;
            TranslationY = tY;
            TranslationZ = tZ;
            RotationX = rX;
            RotationY = rY;
            RotationZ = rZ;
        }

        public static PositionData Create
        (
            double tX = 0,
            double tY = 0,
            double tZ = 0,
            double rX = 0,
            double rY = 0,
            double rZ = 0
        )
        {
            return new PositionData(tX, tY, tZ, rX, rY, rZ);
        }
    }
}

