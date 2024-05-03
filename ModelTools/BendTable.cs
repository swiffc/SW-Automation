using System.Collections.Generic;

namespace ModelTools
{
    public static class BendTable
    {
        public static Dictionary<double, double> R = new Dictionary<double, double>
        {
            //  THK      R
            { 0.1344, 0.2500 },
            { 0.1875, 0.3125 },
            { 0.2500, 0.5000 },
            { 0.3125, 0.5625 },
            { 0.3750, 0.6875 }
        };

        public static double GetBendRadius(double thickness)
        {
            if (R.TryGetValue(thickness, out double radius))
            {
                return radius;
            }

            return thickness * 2; 
        }
    }
}
