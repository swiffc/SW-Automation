using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using HDR.Box;
using HDR.Connections.Derived;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Connections
{
    internal abstract class Flange : Part
    {
        // Constructor
        protected Flange(SW_Assembly parentMainAssembly) : base(parentMainAssembly) 
        {
            var loadPositionData = Position;
        }


        // Static methods
        public static double CalculateYTranslation(double flangeProjectionY, string flangeBundleLocation)
        {
            bool isBottom = flangeBundleLocation.StartsWith("B");
            return (TopBtmPlate.THK + flangeProjectionY) * (isBottom ? -1 : 1) - (isBottom ? Header.BoxHeight : 0);
        }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("O", "sk:Body", FLG.FlangeO);
            EditDimension("Q", "sk:Body", FLG.FlangeQ);
            EditDimension("R", "sk:Body", FLG.FlangeR);
            EditDimension("X", "sk:Body", FLG.FlangeX);
            EditDimension("RD", "sk:Body", FLG.FlangeRD);
            EditDimension("YY", "sk:Body", FLG.FlangeYY);
            EditDimension("OD", "sk:Body", FLG.OD);
            EditDimension("Wall", "sk:Body", FLG.Wall);
            EditDimension("NB", "sk:Hole", FLG.FlangeNB);
            EditDimension("DB", "sk:Hole", FLG.FlangeDB);
            EditDimension("BC", "sk:Hole", FLG.FlangeBC);
        }


        // Property overrides
        public override bool Enabled =>
            (BundleLocation == "TL" && Header == Header61) ||
            (BundleLocation == "TR" && Header == Header62) ||
            (BundleLocation == "BL" && Header == LowestLeftHeader) ||
            (BundleLocation == "BR" && Header == LowestRightHeader);
        public override string StaticPartNo => "Flange";
        public override Shape RawMaterialShape => Shape.None;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                if (_staticPos == null) { _staticPos = new List<PositionData>(); }

                if (_posInlet == null && FLG is InletNozzle && Enabled)
                {
                    _posInlet = NewPositionData();
                    _staticPos.AddRange(_posInlet);
                }
                else if (_posOutlet == null && FLG is OutletNozzle && Enabled)
                {
                    _posOutlet = NewPositionData();
                    _staticPos.AddRange(_posOutlet);
                }

                return _staticPos;
            }
        }


        // Public methods
        public static void ClearPositionData()
        {
            _staticPos = null;
            _posInlet = null;
            _posOutlet = null;
        }


        // Private methods
        List<PositionData> NewPositionData()
        {
            double xTranslation = OffsetX;
            double yTranslation = CalculateYTranslation(ProjectionY, BundleLocation);
            double zTranslation = 0;
            double xRotation = top ? 0 : 180;

            // Seed
            var pos = new List<PositionData>
            {
                PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, xRotation),
            };

            // Pattern
            for (int i = 1; i < Count; i++)
            {
                xTranslation -= Spacing;
                pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, xRotation));
            }

            return pos;
        }


        // Abstract properties
        protected abstract IConnection FLG { get; }


        // Wrapper properties
        string BundleLocation => FLG.Location;
        double OffsetX => FLG.OffsetX;
        double ProjectionY => FLG.ProjectionY;
        double Count => FLG.Count;
        double Spacing => FLG.Spacing;
        bool top => BundleLocation.StartsWith("T");
        bool bottom => BundleLocation.StartsWith("B");


        // Backing fields
        static List<PositionData> _staticPos;
        static List<PositionData> _posInlet;
        static List<PositionData> _posOutlet;
    }
}
