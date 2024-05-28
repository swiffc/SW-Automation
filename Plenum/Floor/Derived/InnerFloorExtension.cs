using ModelTools;
using Plenum.Helpers.Static;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;
using bTools = ModelTools.BendTable;
using Plenum.Floor.Derived.Derived;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor.Derived
{
    internal class InnerFloorExtension : FloorExtension
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return Fan_Count > 1 && FloorPanel.ExtensionRequired ? true : false;
            }
        }
        internal static double NominalLength
        {
            get
            {
                double nominalLength = InnerFloorPanel.GetNominalLength(CallerType);
                double innerFloorPanelLength = InnerFloorPanel.GetLength();
                double floorSpliceLength = FloorSplice.NominalLength / 2;

                double returnValue = nominalLength - innerFloorPanelLength - floorSpliceLength;
                return returnValue;
            }
        }


        // Constructor
        public InnerFloorExtension(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            base.EditDimensions(modelDoc2);

            mTools.EditDimension("Length", "sk:Plate", NominalLength - mTools.AssemblyClearance / 2, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            mTools.SuppressFeatures_Legacy(false, modelDoc2, "ColumnCut", "ColumnCutMirror");
        }


        // Property overrides
        public override string StaticPartNo => "207";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (FloorPanel.ExtensionRequired)
                    {
                        var zTranslation = FanCenter.ZTranslation(CallerType);
                        double yTranslation = Plenum_Depth - Math.Max(EndPanel_THK, SidePanel_THK);
                        double zOffset = InnerFloorPanel.GetLength() + FloorSplice.NominalLength / 2 + mTools.AssemblyClearance;

                        for (int i = 0; i < Fan_Count; i++)
                        {
                            bool isNotLastForNonLegacy = CallerType != Design.Legacy && i != Fan_Count - 1;
                            bool isNotFirstForNonLegacy = CallerType != Design.Legacy && i != 0;

                            if (CallerType == Design.Legacy || isNotLastForNonLegacy)
                                _position.Add(PositionData.Create(tZ: zTranslation[i] - zOffset, tY: -yTranslation));

                            if (CallerType == Design.Legacy || isNotFirstForNonLegacy)
                                _position.Add(PositionData.Create(tZ: zTranslation[i] + zOffset, tY: -yTranslation, rY: 180));
                        }
                    }

                }
                return _position;
            }
        }
    }
}

