using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.AccessControl;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.TextBox;

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

            EditDimension("ColumnCenterToHole", "sk:Plate", Beam_FlangeWidth / 2 + ColumnBoundsToHole - Beam_WebTHK / 2);
            EditDimension("Angle", "sk:Plate", BraceAngle);

        }


        // Property overrides
        public override bool Enabled => new[] { "L", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "104W";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => Clip_THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                double columnCenterToClipHole = Beam_FlangeWidth / 2 + ColumnBoundsToHole;
                double xTranslation = 0;
                double zTranslation = Beams_AreRotated ? EndPanelShift : 0;

                if (ParentSubAssembly.StaticPartNo == "101")
                {
                    if (Beams_AreRotated)
                    {
                        pos.Add(PositionData.Create(tZ: -columnCenterToClipHole, tY: ClipHeight, rY: -90, tX: xTranslation));
                    }
                    else
                    {
                        pos.Add(PositionData.Create(tX: columnCenterToClipHole, tY: ClipHeight, tZ: zTranslation));
                    }
                }

                if (ParentSubAssembly.StaticPartNo == "106")
                {
                    if (Beams_AreRotated)
                    {
                        pos.Add(PositionData.Create(tZ: columnCenterToClipHole, tY: ClipHeight, rY: 90, tX: xTranslation));
                    }
                    else
                    {
                        pos.Add(PositionData.Create(tX: columnCenterToClipHole, tY: ClipHeight, tZ: -zTranslation));
                    }
                }

                if (ParentSubAssembly.StaticPartNo == "111")
                {
                    if (Beams_AreRotated)
                    {
                        pos.Add(PositionData.Create(tZ: -columnCenterToClipHole, tY: ClipHeight, rY: -90, tX: xTranslation));
                        pos.Add(PositionData.Create(tZ: columnCenterToClipHole, tY: ClipHeight, rY: 90, tX: xTranslation));
                    }
                    else
                    {
                        pos.Add(PositionData.Create(tX: columnCenterToClipHole, tY: ClipHeight));
                    }
                }







                return pos;

            }

        }
    }
}
