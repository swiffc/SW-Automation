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

namespace Plenum.Floor.Derived
{
    internal class InnerFloorExtension : FloorExtension
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return FanCount > 1 && FloorPanel.ExtensionRequired ? true : false;
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
        public InnerFloorExtension(CallerType callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            base.EditDimensions(modelDoc2);

            mTools.EditDimension("Length", "sk:Plate", NominalLength - mTools.AssemblyClearance / 2, modelDoc2);
            mTools.EditDimension("Rotate", "sk:ColumnCut", CallerType == CallerType.Johnson ? 90 : 0, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            mTools.SuppressFeatures(false, modelDoc2, "ColumnCut", "ColumnCutMirror");
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
                        double yTranslation = Depth - Math.Max(EndPanel.THK, SidePanel.THK);
                        double zOffset = InnerFloorPanel.GetLength() + FloorSplice.NominalLength / 2 + mTools.AssemblyClearance;

                        for (int i = 0; i < FanCount; i++)
                        {
                            bool isNotLastForNonLegacy = CallerType != CallerType.Legacy && i != FanCount - 1;
                            bool isNotFirstForNonLegacy = CallerType != CallerType.Legacy && i != 0;

                            if (CallerType == CallerType.Legacy || isNotLastForNonLegacy)
                                _position.Add(PositionData.Create(tZ: zTranslation[i] - zOffset, tY: -yTranslation));

                            if (CallerType == CallerType.Legacy || isNotFirstForNonLegacy)
                                _position.Add(PositionData.Create(tZ: zTranslation[i] + zOffset, tY: -yTranslation, rY: 180));
                        }
                    }

                }
                return _position;
            }
        }
    }
}

