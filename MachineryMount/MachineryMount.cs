using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.Base;

namespace MachineryMount
{
    public class MachineryMount : MainAssembly

    {
        /*
        Brands
         Smithco
         Hudson
         CCSC
         Hammco

        Types
         forced
         induced
         gear
         direct
        */


        public MachineryMount(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription)
        {
        }

        public static void Main()
        {

        }


    }
}
