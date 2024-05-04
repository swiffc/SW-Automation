using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class EndColumn : PlenumColumn
    {
        // Static properties
        public new static bool Enabled { get; set; } = true;


        // Constructor
        public EndColumn(Design callerType) : base(callerType) { }


        // Children
        internal EndBeam EndBeam { get; set; }
        internal FlatSeal FlatSeal { get; set; }


        // Property overrides
        public override string StaticPartNo => "117";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double zTranslation = Plenum_Length / 2;
                    double yRotation = CallerType == Design.Standard ? 0 : 90;
                    double yRotation2 = CallerType == Design.Legacy ? 180 : 0;
                    double yRotation3 = CallerType == Design.Standard ? 180 : 0;

                    // Corner column position
                    PositionData southEast = PositionData.Create(tX: Plenum_Width / 2, tZ: zTranslation, rY: -yRotation + yRotation3);
                    PositionData southWest = PositionData.Create(tX: -Plenum_Width / 2, tZ: zTranslation, rY: yRotation + yRotation2);
                    PositionData northEast = PositionData.Create(tX: Plenum_Width / 2, tZ: -zTranslation, rY: -yRotation + yRotation2 + yRotation3);
                    PositionData northWest = PositionData.Create(tX: -Plenum_Width / 2, tZ: -zTranslation, rY: yRotation);

                    // Initialize the column position list with corner columns
                    List<PositionData> positions = new List<PositionData>
                    {
                        southEast,
                        southWest,
                        northEast,
                        northWest
                    };

                    return positions;
                }
                return _position;
            }
        }


        // Method overrides
        protected override void InstantiateSubComponents(AssemblyDoc assemblyDoc)
        {
            base.InstantiateSubComponents(assemblyDoc);

            EndBeam.CallerDoc = assemblyDoc;
            EndBeam = new EndBeam(CallerType);

            if (FlatSeal.Enabled)
            {
                FlatSeal.CallerDoc = assemblyDoc;
                FlatSeal = new FlatSeal(CallerType);
            }
        }
        protected override void PlaceSubComponents(AssemblyDoc assemblyDoc)
        {
            base.PlaceSubComponents(assemblyDoc);

            FTools.PlaceComponent(EndBeam, assemblyDoc);

            if (FlatSeal.Enabled)
                FTools.PlaceComponent(FlatSeal, assemblyDoc);

        }
    }

}
