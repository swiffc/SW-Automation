using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using System.Linq;
using System.Security.AccessControl;
using static FileTools.SharedProperties;

namespace Structure.Columns.Derived.Children.Derived
{
    internal class FlangeClip : Clip
    {
        // Static properties
        static public double zTranslation => Beam.Depth / 2 + ColumnBoundsToHole;


        // Constructor
        public FlangeClip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            base.Dimensions();

            EditDimension("ColumnCenterToHole", "sk:Plate", ColumnBoundsToHole);
            EditDimension("Angle", "sk:Plate", BraceAngle);
        }


        // Property overrides
        public override bool Enabled => new[] { "L", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "104F";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();
                bool isRotated = Beam.IsRotated;
                double translationY = ClipHeight;

                switch (ParentSubAssembly.StaticPartNo)
                {
                    case "101":
                        if (isRotated)
                        {
                            pos.Add(PositionData.Create(tY: translationY, tX: zTranslation, rY: -90));
                        }
                        else
                        {
                            pos.Add(PositionData.Create(tY: translationY, tZ: -zTranslation, rY: 180));
                        }
                        break;

                    case "106":
                        if (isRotated)
                        {
                            pos.Add(PositionData.Create(tY: translationY, tX: zTranslation, rY: -90));
                        }
                        else
                        {
                            pos.Add(PositionData.Create(tY: translationY, tZ: zTranslation));
                        }
                        break;

                    case "111":
                        if (isRotated)
                        {
                            pos.Add(PositionData.Create(tY: translationY, tX: zTranslation, rY: -90));
                        }
                        else
                        {
                            pos.Add(PositionData.Create(tY: translationY, tZ: -zTranslation, rY: 180));
                            pos.Add(PositionData.Create(tY: translationY, tZ: zTranslation));
                        }
                        break;
                }

                return pos;
            }

        }
    }
}
