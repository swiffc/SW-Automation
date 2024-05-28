using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveWeldment.Children
{
    internal class CrossStiffenerC : Part
    {
        // Constructor
        public CrossStiffenerC(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", DriveFrame.Width);
        }
        protected override void Features()
        {
            EditFeature_StructuralMemberSize(Stringer.Size);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "237";
        public override Shape RawMaterialShape => Shape.Channel;
        public override string SizeOrThickness => Stringer.Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: DriveFrame.Width/2)
                };
            }
        }
    }
}
