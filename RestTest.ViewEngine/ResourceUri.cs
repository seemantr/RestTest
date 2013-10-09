namespace RestTest.ViewEngine
{
    using System.Collections.Generic;

    using RestSharp;

    public class ResourceUri
    {
        #region Public Properties

        public string Description { get; set; }
        public string Name { get; set; }
        public string Note { get; set; }
        public Dictionary<string, string> StatusCodes { get; set; }
        public List<ResourceParam> ResourceParams { get; set; }
        public List<ResourceExample> Examples { get; set; }
        public Method Method { get; set; }
        public string Request { get; set; }

        public ResourceUri()
        {
            this.StatusCodes = new Dictionary<string, string>();
            this.Examples = new List<ResourceExample>();
            this.ResourceParams = new List<ResourceParam>();
        }

        #endregion
    }
}