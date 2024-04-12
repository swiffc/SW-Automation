using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FileTools.Base
{

    public interface IComponentInfo2 : IComponentInfo
    {
        bool Enabled { get; }
    }

}
