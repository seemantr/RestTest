namespace RestTest.ViewEngine
{
    using System.Collections.Generic;

    using RestSharp;

    public class Assert
    {
        public List<string> Values { get; set; }
        public AssertType AssertType { get; set; }

        public Assert()
        {
            this.Values = new List<string>();
        }
    }

    public enum AssertType
    {
        DeepEquals,
        Equals,
        Exists
    }
}