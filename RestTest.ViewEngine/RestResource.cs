namespace RestTest.ViewEngine
{
    using System.Collections.Generic;

    public class RestResource
    {
        #region Public Properties
        public string ResourceDescription { get; set; }
        public string ResourceName { get; set; }
        public string Note { get; set; }
        public List<ResourceParam> Params { get; set; }
        public List<ResourceUri> Uris { get; set; }

        public RestResource()
        {
            Uris = new List<ResourceUri>();
            Params = new List<ResourceParam>();
        }

        #endregion
    }
}