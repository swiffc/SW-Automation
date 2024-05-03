using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using static FileTools.Base.Part;

namespace FileTools
{
    public static partial class RawMaterial_2
    {


        public static Dictionary<(string THK, Spec), (string RM, string Description)> JDE = new Dictionary<(string THK, Spec), (string RM, string Description)>
        {
            // Sheet
            { ("0.1344",            Spec.A1011_33),     ("11110-HPC",    "SHEET_10GA_A1011-33") },


            // Plate
            { ("0.1875",            Spec.A36),          ("11399",       "PLATE_3/16\"_A36") },
            { ("0.1875",            Spec.A572_50),      ("60015-HPC",   "PLATE_3/16\"_A572_50") },

            { ("0.25",              Spec.A36),          ("11449-HPC",   "PLATE_1/4\"_A36_72\"WIDE") },
            { ("0.25",              Spec.A572_50),      ("60038",       "PLATE_1/4\"_A572_50") },

            { ("0.3125",            Spec.A36),          ("11519",       "PLATE_5/16\"_A36") },
            { ("0.3125",            Spec.A572_50),      ("54877-HPC",   "PLATE_5/16\"_A572_50") },

            { ("0.375",             Spec.A36),          ("11569",       "PLATE_3/8\"_A36") },
            { ("0.375",             Spec.A572_50),      ("59500",       "PLATE_3/8\"_A572_50") },

            { ("0.5",               Spec.A36),          ("11619",       "PLATE_1/2\"_A36") },
            { ("0.5",               Spec.A572_50),      ("54397",       "PLATE_1/2\"_A572_50") },


            // Beam
<<<<<<< HEAD
            { ("6x15",             Spec.A992),         ("13011-HPC",   "BEAM_W_6x15_A992") },
            { ("6x20",             Spec.A992),         ("13012-HPC",   "BEAM_W_6x20_A992") },
            { ("6x25",             Spec.A992),         ("13015-HPC",   "BEAM_W_6x25_A992") },
            { ("8x31",             Spec.A992),         ("13027-HPC",   "BEAM_W_8x31_A992") },
=======
            { ("W6x15",             Spec.A992),         ("13011-HPC",   "BEAM_W_6x15_A992") },
            { ("W6x20",             Spec.A992),         ("13012-HPC",   "BEAM_W_6x20_A992") },
            { ("W6x25",             Spec.A992),         ("13015-HPC",   "BEAM_W_6x25_A992") },
            { ("W8x31",             Spec.A992),         ("13027-HPC",   "BEAM_W_8x31_A992") },
>>>>>>> releases/v4.0.0


            // Angle
            { ("2.5x2.5x0.1875",   Spec.A36),          ("12021",       "ANGLE_2-1/2\"x2-1/2\"x3/16\"_A36") },
            { ("2.5x2.5x0.1875",   Spec.A572_50),      ("30592",       "ANGLE_2-1/2\"x1-1/2\"x3/16\"_A572 50") },

            { ("3x2x0.1875",       Spec.A36),          ("12132",       "ANGLE_3\"x2\"x3/16\"_A36") },
            { ("3x2x0.1875",       Spec.A572_50),      ("60014-HPC",   "ANGLE_3\"x2\"x3/16\"_A572-50") },

            { ("3x3x0.1875",       Spec.A36),          ("12026",       "ANGLE_3\"x3\"x3/16\"_A36") },
            { ("3x3x0.1875",       Spec.A572_50),      ("54949",       "ANGLE_3\"x3\"x3/16\"_A572-50") },
        };
    }
}
