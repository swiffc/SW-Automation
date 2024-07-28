using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using HDR.Box;
using HDR.Box.Derived;
using HDR.Connections.Derived;
using ModelTools;
using System;
using System.CodeDom;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;
using static System.Net.Mime.MediaTypeNames;
using static FileTools.Base.SW_Assembly;

namespace HDR.Connections
{
    internal abstract class Flange : Part
    {
        // Constructor
        protected Flange(SW_Assembly parentMainAssembly) : base(parentMainAssembly) 
        {
            var loadPositionData = Position;

            if (IdenticalFlanges)
                DontProcessLocation.Add(typeof(OutletFlange));
        }


        // Private properties
        bool IdenticalFlanges => Inlet.FlangePartNo == Outlet.FlangePartNo;


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
            (FLG.Location == "TL" && Header == Header61) ||
            (FLG.Location == "TR" && Header == Header62) ||
            (FLG.Location == "BL" && Header == LowestLeftHeader) ||
            (FLG.Location == "BR" && Header == LowestRightHeader);
        public override string StaticPartNo => "Flange";
        public override Shape RawMaterialShape => Shape.None;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                if (_staticPos == null) _staticPos = new List<PositionData>();

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

                if (IdenticalFlanges)
                    return _staticPos;
                else if (FLG is InletNozzle)
                    return _posInlet;
                else if (FLG is OutletNozzle)
                    return _posOutlet;
                else throw new Exception("Flange is neither Inlet nor Outlet.");
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
        protected List<PositionData> NewPositionData()
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
        protected string BundleLocation => FLG.Location;
        protected double OffsetX => FLG.OffsetX;
        protected double ProjectionY => FLG.ProjectionY;
        protected double Count => FLG.Count;
        protected double Spacing => FLG.Spacing;
        protected bool top => BundleLocation.StartsWith("T");
        protected bool bottom => BundleLocation.StartsWith("B");


        // Backing fields
        static List<PositionData> _staticPos;
        static List<PositionData> _posInlet;
        static List<PositionData> _posOutlet;
    }
}
