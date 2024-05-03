using Microsoft.Win32;
using ModelTools;
using Plenum.Helpers.Static;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using System.Windows.Media;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;

namespace Plenum
{
    internal class MotorBeamWld : IComponentInfo
    {
        // Static properties
        public static bool Enabled { get; set; } = true;


        // Constructor
        public MotorBeamWld(CallerType callerType)
        {
            CallerType = callerType;
            InitializeComponents();
        }


        // Private methods
        private void InitializeComponents()
        {
            AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, true);

            MotorBeamPart = new MotorBeamPart(CallerType);
            MotorBeamPlate = new MotorBeamPlate(CallerType);


            Component2[] userLocatedComponents = aTools.UnfixedComponentsArray(AssemblyDoc);
            FTools.PlaceComponent(MotorBeamPart, AssemblyDoc);
            FTools.PlaceComponent(MotorBeamPlate, AssemblyDoc);
            aTools.FixComponentLocations(userLocatedComponents, AssemblyDoc);

            cTools.Release(AssemblyDoc);
        }


        // Children
        internal MotorBeamPart MotorBeamPart { get; set; }
        internal MotorBeamPlate MotorBeamPlate { get; set; }


        // Public properties
        public string FilePath
        {
            get
            {
                if (_filePath == null)
                {
                    var partNo = PartNo;

                    _filePath = FTools.GetFilePath(PartNo, FileType); ;
                }
                return _filePath;
            }
        }
        public string PartNo
        {
            get
            {
                if (_partNo == null)
                {
                    _partNo = FTools.GetPartNoFromAssembly(StaticPartNo, Plenum.AssemblyDoc)
                             ?? aTools.GetPartNoFromDirectory(StaticPartNo, Plenum.AssemblyDoc)
                             ?? FTools.CreateNew_ComponentFile(StaticPartNo);
                }
                return _partNo;
            }
        }
        public List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    List<double> zTranslation = FanCenter.ZTranslation(CallerType);
                    double yTranslation = 6;

                    _position = new List<PositionData>();
                    for (int i = 0; i < FanCount; i++)
                    {
                        _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation));
                    }
                }

                return _position;
            }
        }


        // Internal properties
        internal static AssemblyDoc AssemblyDoc { get; set; }
        private static double _length;

        public static double LocalLength
        {
            get
            {
                switch (CallerType)
                {
                    case CallerType.Standard:
                        return Width - mTools.AssemblyClearance * 2;
                    case CallerType.Johnson:
                        return Width + Beam.Depth - mTools.AssemblyClearance * 2;
                    case CallerType.Legacy:
                        return Width + Beam.Depth - Beam.FlangeTHK * 2 - SidePanel.THK * 2 - mTools.AssemblyClearance * 2;
                }
                return _length;
            }
        }



        // Private properties
        private static CallerType CallerType { get; set; }
        private string _filePath;
        private string _partNo;
        private List<PositionData> _position;


        // Constants
        public string StaticPartNo { get; set; } = "266";
        private const string FileType = "SLDASM";
    }
}
