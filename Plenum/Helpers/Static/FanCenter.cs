using ModelTools;
using System.Collections.Generic;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Helpers.Static
{
    public static class FanCenter
    {
        public static List<double> ZTranslation(Design callerType)
        {
            var zList = new List<double>();
            double zTranslation = (Plenum_Length / 2) - (Plenum_Length / (2 * Fan_Count));

            // Skip first fan if Johnson unless it's one fan
            if (callerType != Design.Johnson || Fan_Count == 1)
                zList.Add(zTranslation);

            // Locate other fans if any
            for (int i = 1; i < Fan_Count; i++)
            {
                if (i == Fan_Count - 1 && callerType == Design.Johnson)
                    continue; // Skip last Johnson fan

                zTranslation -= Plenum_Length / Fan_Count;
                zList.Add(zTranslation);
            }

            // Add in the skipped Johnson locations
            if (callerType == Design.Johnson && Fan_Count != 1)
            {
                zTranslation = (Plenum_Length / 2) + Johnson.ExtraLength - ((Plenum_Length / Fan_Count) + Johnson.ExtraLength) / 2;
                zList.Insert(0, zTranslation);
                zList.Add(-zTranslation);
            }
            return zList;
        }
    }
}
