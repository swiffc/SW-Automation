using Bundle.AirSeals;
using Bundle.SideFrame.Derived.Children;
using Bundle.TubeSupports;
using Bundle.TubeSupports.Children;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.TubeKeepers
{
    internal class TubeKeeperBent : Part
    {
        // Static properties
        static public double BottomToSlotCenter => 1.5;


        // Constructor
        public TubeKeeperBent(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }
        public TubeKeeperBent(int parentNone) : base(parentNone) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Backing", AirSeal.Length);
        }


        // Property overrides
        public override bool Enabled => IsSmithco;
        public override string StaticPartNo => "1504B";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => "0.1344";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    _pos = TubeSupport.CalculateKeeperPositionFrom(TubeSupport.PositionDataList);
                }
                return _pos;
            }
        }



    }
}
