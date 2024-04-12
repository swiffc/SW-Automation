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
using static FileTools.FileTools;

namespace Plenum.Floor.Derived.Derived
{
    internal class OuterFloorExtension : FloorExtension
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return FloorPanel.ExtensionRequired ? true : false;
            }
        }


        // Constructor
        public OuterFloorExtension(CallerType callerType) : base(callerType) { }


        // Property overrides
        public override string StaticPartNo => "206";
        public override RawMaterial Shape => base.Shape;
        public override string Size => FloorPanel.THK.ToString();
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (CallerType != CallerType.Legacy && FloorPanel.ExtensionRequired)
                    {
                        var zTranslation = FanCenter.ZTranslation(CallerType);
                        double yTranslation = Depth - Math.Max(EndPanel.THK, SidePanel.THK);
                        int i = FanCount == 1 ? 0 : FanCount - 1;
                        double zOffset = OuterFloorPanel.GetLength() + FloorSplice.NominalLength / 2 + mTools.AssemblyClearance;

                        _position.Add(PositionData.Create(tZ: zTranslation[0] + zOffset, tY: -yTranslation));
                        _position.Add(PositionData.Create(tZ: zTranslation[i] - zOffset, tY: -yTranslation, rY: 180));
                    }
                }

                return _position;
            }
        }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            base.EditDimensions(modelDoc2);

            mTools.EditDimension("Length", "sk:Plate", NominalLength - mTools.AssemblyClearance, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            OuterFloorPanel.GetLengthAndRotation(out double zColumnCut, out _);

            double nominalLength = OuterFloorPanel.GetNominalLength(CallerType);
            double adjustedLength = OuterFloorPanel.CheckSectioningRequirements(nominalLength, OuterFloorPanel.GetWidth(CallerType));

            double length = adjustedLength + (FloorPanel.SpliceRequired ? FloorSplice.NominalLength / 2 : 0);

            if (CallerType == CallerType.Johnson && zColumnCut + Beam.FlangeWidth / 2 < length)
                mTools.SuppressFeatures(true, modelDoc2, "ColumnCut", "JohnsonCut");
            else
                mTools.SuppressFeatures(false, modelDoc2, "ColumnCut", "JohnsonCut");
        }

        // Private properties
        internal static double NominalLength
        {
            get
            {
                return OuterFloorPanel.GetNominalLength(CallerType) - OuterFloorPanel.GetLength() - FloorSplice.NominalLength / 2;
            }
        }
    }
}
