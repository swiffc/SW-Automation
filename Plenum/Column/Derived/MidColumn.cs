using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class MidColumn : PlenumColumn
    {
        // Static properties
        public new static bool Enabled
        {
            get
            {
                return Fan_Count > 1 ? true : false;
            }
        }


        // Constructor
        public MidColumn(Design callerType) : base(callerType) { }


        // Children
        internal MidBeam MidBeam { get; set; }


        // Property overrides
        public override string StaticPartNo => "116";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double zTranslation = Plenum_Length / 2;
                    double yRotation = CallerType == Design.Standard ? 0 : 90;
                    double flip = CallerType == Design.Standard ? 180 : 0;

                    List<PositionData> positions = new List<PositionData>();

                    if (Fan_Count > 1 && Mid_Columns)
                    {
                        for (int i = 1; i < Fan_Count; i++)
                        {
                            zTranslation -= Plenum_Length / Fan_Count;
                            PositionData midEast = PositionData.Create(tX: Plenum_Width / 2, tZ: zTranslation, rY: -yRotation + flip);
                            PositionData midWest = PositionData.Create(tX: -Plenum_Width / 2, tZ: zTranslation, rY: yRotation);

                            positions.Add(midEast);
                            positions.Add(midWest);
                        }
                    }

                    return positions;
                }
                return _position;
            }
        }


        // Method overrides
        protected override void InstantiateSubComponents(AssemblyDoc assemblyDoc)
        {
            base.InstantiateSubComponents(assemblyDoc);

            MidBeam.CallerDoc = assemblyDoc;
            MidBeam = new MidBeam(CallerType);
        }
        protected override void PlaceSubComponents(AssemblyDoc assemblyDoc)
        {
            base.PlaceSubComponents(assemblyDoc);

            FTools.PlaceComponent(MidBeam, assemblyDoc);
        }
    }

}