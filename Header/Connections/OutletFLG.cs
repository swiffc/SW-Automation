using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;
using static FileTools.CommonData.CommonData.Outlet;
using HDR.Box;

namespace HDR.Connections
{
    internal class OutletFLG : Part
    {
        // Constructor
        public OutletFLG(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("O", "sk:Body", OutletFlange.O);
            EditDimension("Q", "sk:Body", OutletFlange.Q);
            EditDimension("R", "sk:Body", OutletFlange.R);
            EditDimension("X", "sk:Body", OutletFlange.X);
            EditDimension("RD", "sk:Body", OutletFlange.RD);
            EditDimension("YY", "sk:Body", OutletFlange.YY);
            EditDimension("OD", "sk:Body", OutletFlange.OD);
            EditDimension("Wall", "sk:Body", OutletFlange.Wall);
            EditDimension("NB", "sk:Hole", OutletFlange.NB);
            EditDimension("DB", "sk:Hole", OutletFlange.DB);
            EditDimension("BC", "sk:Hole", OutletFlange.BC);
        }


        // Property overrides
        public override string PartNo => "OutletFlange";
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
        string Location => OutletFlange.Location;
        double OffsetX => OutletFlange.OffsetX;
        double ExtensionY => OutletFlange.ExtensionY;
        double Count => OutletFlange.Count;
        double Spacing => OutletFlange.Spacing;
    }
}
