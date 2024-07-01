using Bundle.Misc;
using Bundle.TubeSupports;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace Bundle.TubeKeepers
{
    internal class TubeKeeperWeldment : SubAssembly
    {
        // Static properties
        static public bool IsLarge => Bundle.Width > 13 * 12;


        // Constructor
        public TubeKeeperWeldment(SW_Assembly parentAssembly) : base(parentAssembly) { }
        public TubeKeeperWeldment(int parentNone) : base(parentNone) { }


        // Property overrides
        public override bool Enabled => !IsSmithco;
        public override string StaticPartNo => "1504W";
        public override List<PositionData> Position
        {
            get
            {
                return TubeSupport.CalculateKeeperPositionFrom(TubeSupport.PositionDataList);
            }
        }

    }
}
