using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ModelTools
{
    public static class MotorFrame
    {
        public static Dictionary<double, (double O, double D, double BA, double C, double NW, double E)> Motor = new
            Dictionary<double, (double O, double D, double BA, double C, double NW, double E)>
        {
        //    size      O       D      BA     C      NW     E
            { 143,    ( 6.93,   3.5,   2.25,  14.0,  2.25,  2.75 ) },
            { 145,    ( 6.93,   3.5,   2.25,  14.0,  2.25,  2.75 ) },
            { 182,    ( 8.86,   4.5,   2.7,   14.8,  2.75,  3.75 ) },
            { 184,    ( 8.86,   4.5,   2.75,  16.2,  2.75,  3.75 ) },
            { 213,    ( 10.62,  5.25,  5.25,  20.3,  3.38,  4.25 ) },
            { 215,    ( 10.62,  5.25,  5.25,  20.3,  3.38,  4.25 ) },
            { 254,    ( 12.62,  6.25,  4.25,  25.8,  4.0,   5.0  ) },
            { 256,    ( 12.62,  6.25,  4.25,  25.8,  4.0,   5.0  ) },
            { 284,    ( 14.19,  7.0,   4.75,  29.4,  4.63,  5.5  ) },
            { 286,    ( 14.19,  7.0,   4.75,  29.4,  4.63,  5.5  ) },
            { 324,    ( 15.94,  8.0,   5.25,  32.1,  5.25,  6.25 ) },
            { 326,    ( 15.94,  8.0,   5.25,  32.1,  5.25,  6.25 ) },
            { 364,    ( 17.81,  9.0,   5.88,  35.6,  5.88,  7.0  ) },
            { 365,    ( 17.81,  9.0,   5.88,  35.6,  5.88,  7.0  ) },
            { 404,    ( 19.9,   10.0,  6.25,  39.5,  7.25,  8.0  ) },
            { 405,    ( 19.9,   10.0,  6.25,  39.5,  7.25,  8.0  ) }
        };
    }
}
