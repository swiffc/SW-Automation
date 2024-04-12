using ModelTools;
using Plenum.JohnsonBeam.Children;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;

namespace Plenum.JohnsonBeam
{
    internal class JohnsonBeamWld : Assembly
    {
        // Static properties
        public static bool Enabled { get; set; } = false;
        internal new static AssemblyDoc AssemblyDoc { get; set; }
        internal static double Length => Width - Beam.Depth;


        // Constructor
        public JohnsonBeamWld(CallerType callerType) : base(callerType) { }


        // Method overrides
        protected override void InstantiateSubComponents(AssemblyDoc assemblyDoc)
        {
            AssemblyDoc = base.AssemblyDoc;
            JohnsonBeamPart = new JohnsonBeamPart(CallerType);
            JohnsonBeamPlate = new JohnsonBeamPlate(CallerType);
        }
        protected override void PlaceSubComponents(AssemblyDoc assemblyDoc)
        {
            FTools.PlaceComponent(JohnsonBeamPart, AssemblyDoc);
            FTools.PlaceComponent(JohnsonBeamPlate, AssemblyDoc);
        }


        // Children
        internal JohnsonBeamPart JohnsonBeamPart { get; set; }
        internal JohnsonBeamPlate JohnsonBeamPlate { get; set; }


        // Property overrides
        public override string StaticPartNo => "113";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (CallerType == CallerType.Johnson)
                {
                    double zTranslation = Plenum.Length / 2;

                    _position.Add(PositionData.Create(tZ: zTranslation));
                    _position.Add(PositionData.Create(tZ: -zTranslation, rY: 180));
                }

                return _position;
            }
        }
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
    }
}
