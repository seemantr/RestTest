namespace RestTest.ViewEngine
{
    using System.Collections.Generic;

    public class RestDocument
    {
        public string Url { get; set; }
        public string DocumentDescription { get; set; }
        public string DocumentName { get; set; }
        public string Version { get; set; }
        public string ApiVersion { get; set; }
        public List<string> RequestFormat { get; set; }
        public List<string> ResponseFormat { get; set; }
        public List<RestResource> Resources { get; set; }

        public RestDocument()
        {
            this.Resources = new List<RestResource>();
            this.RequestFormat = new List<string>();
            this.ResponseFormat = new List<string>();
        }
    }
}