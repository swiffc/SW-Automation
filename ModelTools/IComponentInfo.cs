using System.Collections.Generic;

namespace ModelTools
{
    public interface IComponentInfo
    {
        string PartNo { get; }
        string FilePath { get; }
        List<PositionData> Position { get; }
        string StaticPartNo { get; }
    }
}
