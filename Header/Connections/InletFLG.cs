using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;
using static FileTools.CommonData.CommonData.Inlet;
using HDR.Box;

namespace HDR.Connections
{
    internal class InletFLG : Part
    {
        // Constructor
        public InletFLG(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("O", "sk:Body", InletFlange.O);
            EditDimension("Q", "sk:Body", InletFlange.Q);
            EditDimension("R", "sk:Body", InletFlange.R);
            EditDimension("X", "sk:Body", InletFlange.X);
            EditDimension("RD", "sk:Body", InletFlange.RD);
            EditDimension("YY", "sk:Body", InletFlange.YY);
            EditDimension("OD", "sk:Body", InletFlange.OD);
            EditDimension("Wall", "sk:Body", InletFlange.Wall);
            EditDimension("NB", "sk:Hole", InletFlange.NB);
            EditDimension("DB", "sk:Hole", InletFlange.DB);
            EditDimension("BC", "sk:Hole", InletFlange.BC);
        }


        // Property overrides
        public override string PartNo => "InletFlange";
        public override bool Enabled =>
            (Location == "TL" && Header == Header61) ||
            (Location == "TR" && Header == Header62) ||
            (Location == "BL" && Header == LowestLeftHeader) ||
            (Location == "BR" && Header == LowestRightHeader);
        public override string StaticPartNo => "Flange";
        public override Shape RawMaterialShape => Shape.None;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    bool top = Location.StartsWith("T");
                    bool bottom = Location.StartsWith("B");

                    double xTranslation = OffsetX;
                    double yTranslation = (TopBtmPlate.THK + ExtensionY) * (bottom ? -1 : 1) - (bottom ? Header.BoxHeight : 0);
                    double zTranslation = 0;
                    double xRotation = top ? 0 : 180;

                    _pos = new List<PositionData>
                        {
                            PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, xRotation),
                        };

                    for (int i = 1; i < Count; i++)
                    {
                        xTranslation -= Spacing;
                        _pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, xRotation));
                    }
                }
                return _pos;
            }
        }


        // Wrapper properties
        string Location => InletFlange.Location;
        double OffsetX => InletFlange.OffsetX;
        double ExtensionY => InletFlange.ExtensionY;
        double Count => InletFlange.Count;
        double Spacing => InletFlange.Spacing;
    }
}
