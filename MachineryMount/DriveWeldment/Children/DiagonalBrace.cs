using FileTools.Base;
using MachineryMount.MotorMount.Children;

namespace MachineryMount.DriveWeldment.Children
{
    internal abstract class DiagonalBrace : Part
    {
        // Constructor
        protected DiagonalBrace(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }

        protected double MaxUnreinforcedDistance => 48;
        protected double LongDistance => (Stringer.Length / 2) - (DriveFrame.Width / 2) - CrossStiffenerC.WebTHK - HangerPRC.FlangeDepth;
        protected double ShortDistance => (Stringer.Length / 2) - (MotorMountPart.DriveCenterToBackingExterior + MotorMountPart.BeltTensioningAdjustment + HangerPRC.FlangeDepth + CrossStiffenerC.WebTHK);

    }
}
