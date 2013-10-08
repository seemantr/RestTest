namespace RestTest.ViewEngine
{
    using System.Collections.Generic;

    public class ResourceExample
    {
        public string Name { get; set; }
        public string Note { get; set; }
        public string Description { get; set; }
        public Dictionary<string, string> Headers { get; set; }
        public Dictionary<string, string> QueryParams { get; set; }
        public string Body { get; set; }
        public List<KeyValuePair<string, string>> Asserts { get; set; }

    }
}