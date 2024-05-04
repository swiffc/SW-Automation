using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal abstract class PlenumColumn : Assembly
    {
        public static bool Enabled { get; set; } = true;
        // Static properties
        internal static string Size { get; set; } = "W6x15";


        // Constructor
        protected PlenumColumn(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void InstantiateSubComponents(AssemblyDoc assemblyDoc)
        {
            CapPlate.CallerDoc = assemblyDoc;
            Plate = new CapPlate(CallerType);

            if (JohnsonTopPlate.Enabled)
            {
                JohnsonTopPlate.CallerDoc = assemblyDoc;
                JohnsonTopPlate = new JohnsonTopPlate(CallerType);
            }
        }
        protected override void PlaceSubComponents(AssemblyDoc assemblyDoc)
        {
            FTools.PlaceComponent(Plate, assemblyDoc);

            if (JohnsonTopPlate.Enabled)
                FTools.PlaceComponent(JohnsonTopPlate, assemblyDoc);
        }


        // Property overrides
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;


        // Children
        internal CapPlate Plate { get; set; }
        internal JohnsonTopPlate JohnsonTopPlate { get; set; }
    }
}
