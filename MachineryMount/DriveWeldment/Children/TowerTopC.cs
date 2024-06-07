using FileTools.Base;
using MachineryMount.DriveAssembly;
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
    internal class TowerTopC : Part
    {
        // Constructor
        public TowerTopC(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", DriveFrame.Width);

            EditDimension("Diameter", "sk:BearingHoles", Bearing.Top.HoleDiameter);
            EditDimension("Spacing", "sk:BearingHoles", Bearing.Top.HoleToHoleSpacing);
        }
        protected override void Features()
        {
            EditFeature_StructuralMemberSize(TowerSideC.Size);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "227";
        public override Shape RawMaterialShape => Shape.Channel;
        public override string SizeOrThickness => TowerSideC.Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: DriveFrame.TowerHeight),
                };
            }
        }
    }
}
