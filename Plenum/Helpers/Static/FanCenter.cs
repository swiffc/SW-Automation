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
            double zTranslation = (Length / 2) - (Length / (2 * FanCount));

            // Skip first fan if Johnson unless it's one fan
            if (callerType != Design.Johnson || FanCount == 1)
                zList.Add(zTranslation);

            // Locate other fans if any
            for (int i = 1; i < FanCount; i++)
            {
                if (i == FanCount - 1 && callerType == Design.Johnson)
                    continue; // Skip last Johnson fan

                zTranslation -= Length / FanCount;
                zList.Add(zTranslation);
            }

            // Add in the skipped Johnson locations
            if (callerType == Design.Johnson && FanCount != 1)
            {
                zTranslation = (Length / 2) + Johnson.ExtraLength - ((Length / FanCount) + Johnson.ExtraLength) / 2;
                zList.Insert(0, zTranslation);
                zList.Add(-zTranslation);
            }
            return zList;
        }
    }
}
