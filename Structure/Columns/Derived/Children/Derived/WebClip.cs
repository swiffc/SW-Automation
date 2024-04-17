using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.AccessControl;
using System.Text;
using System.Threading.Tasks;
using static FileTools.SharedProperties;

namespace Structure.Columns.Derived.Children.Derived
{
    internal class WebClip : Clip
    {
        // Constructor
        public WebClip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            base.Dimensions();

            EditDimension("ColumnCenterToHole", "sk:Plate", Beam.FlangeWidth / 2 + ColumnBoundsToHole - Beam.WebTHK / 2);
        }


        // Property overrides
        public override bool Enabled => new[] { "L", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "104W";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();
                double xzTranslation = Beam.FlangeWidth / 2 + ColumnBoundsToHole;
                double translationY = ClipHeight;

                void addPosition(bool rotated, double translation, double rotationY = 0)
                {
                    if (rotated)
                    {
                        pos.Add(PositionData.Create(tZ: translation, tY: translationY, rY: rotationY));
                    }
                    else
                    {
                        pos.Add(PositionData.Create(tX: translation, tY: translationY));
                    }
                }

                switch (ParentSubAssembly.StaticPartNo)
                {
                    case "101":
                        addPosition(Beam.IsRotated, Beam.IsRotated ? -xzTranslation : xzTranslation, Beam.IsRotated ? -90 : 0);
                        break;
                    case "106":
                        addPosition(Beam.IsRotated, xzTranslation, Beam.IsRotated ? 90 : 0);
                        break;
                    case "111":
                        if (Beam.IsRotated)
                        {
                            addPosition(true, -xzTranslation, -90);
                            addPosition(true, xzTranslation, 90);
                        }
                        else
                        {
                            addPosition(false, xzTranslation);
                        }
                        break;
                }

                return pos;
            }

        }
    }
}
