using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RestTest.ViewEngine
{
    using System.IO;

    using RazorEngine;

    public class Hepler
    {
        public static void RenderHtml(RestDocument resource)
        {
            var template = File.ReadAllText(AppDomain.CurrentDomain.BaseDirectory + "\\Template.html");
            var result = Razor.Parse(template, resource);
            File.WriteAllText(AppDomain.CurrentDomain.BaseDirectory + "\\Results.html", result);
        }
    }
}
