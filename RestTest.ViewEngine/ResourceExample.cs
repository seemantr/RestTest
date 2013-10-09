namespace RestTest.ViewEngine
{
    using System;
    using System.Collections.Generic;

    using RestSharp;

    public class ResourceExample
    {
        public string Name { get; set; }
        public string Note { get; set; }
        public string Description { get; set; }
        public Dictionary<string, string> Headers { get; set; }
        public Dictionary<string, string> QueryParams { get; set; }
        public string Body { get; set; }
        public List<Assert> Asserts { get; set; }
        public RestResponse Response { get; set; }

        public ResourceExample()
        {
            this.Asserts =  new List<Assert>();
            this.Headers = new Dictionary<string, string>();
            this.QueryParams = new Dictionary<string, string>();
        }
    }
}