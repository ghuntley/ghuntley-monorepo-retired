// This is the title for your window tab, and your Radar
document.title = "";

// This is the concentic circles that want on your radar
var radar_arcs = [
                   {'r': 100, 'name': 'Adopt'}
                  ,{'r': 200, 'name': 'Trial'}
                  ,{'r': 300, 'name': 'Assess'}
                  ,{'r': 400, 'name': 'Hold'}
//                ,{'r': 500, 'name': 'Possible Extra if you want it'}
                 ];

// This is your raw data
//
// Key
//
// movement:
//   t = moved
//   c = stayed put
//
// blipSize:
//  intValue; This is optional, if you omit this property, then your blip will be size 70.
//            This give you the ability to be able to indicate information by blip size too
//
// url:
// StringValue : This is optional, If you add it then your blips will be clickable to some URL
//
// pc: polar coordinates
//     r = distance away from origin ("radial coordinate")
//     - Each level is 100 points away from origin
//     t = angle of the point from origin ("angular coordinate")
//     - 0 degrees is due east
//
// Coarse-grained quadrants
// - Techniques: elements of a software development process, such as experience design; and ways of structuring software, such micro-services.
// - Tools: components, such as databases, software development tools, such as versions control systems; or more generic categories of tools, such as the notion of polyglot persistance.
// - Platforms: things that we build software on top of: mobile technologies like Android, virtual platforms like the JVM, or generic kinds of platforms like hybrid clouds
// - Programming Languages and Frameworks
//
// Rings:
// - Adopt: blips you should be using now; proven and mature for use
// - Trial: blips ready for use, but not as completely proven as those in the adopt ring; use on a trial basis, to decide whether they should be part of your toolkit
// - Assess: things that you should look at closely, but not necessarily trial yet - unless you think they would be a particularly good fit for you
// - Hold: things that are getting attention in the industry, but not ready for use; sometimes they are not mature enough yet, sometimes they are irredeemably flawed
//      Note: there's no "avoid" ring, but throw things in the hold ring that people shouldn't use.

var h = 1000;
var w = 1200;

var radar_data = [
    { "quadrant": "Techniques",
        "left" : 45,
        "top" : 18,
        "color" : "#8FA227",
        "items" : [

{'name': 'Remote Work',                pc: {r: 25, t: 150}, movement: 't'},
{'name': 'Mathematics',                pc: {r: 50, t: 130}, movement: 'c'},
{'name': 'Logic',                      pc: {r: 60, t: 140}, movement: 'c'},
{'name': 'Code Reviews',               pc: {r: 70, t: 120}, movement: 'c'},
{'name': 'Type Theory',                pc: {r: 70, t: 150}, movement: 't'},
{'name': 'Property based testing',     pc: {r: 70, t: 165}, movement: 't'},
{'name': 'Continuous Flow', pc: {r: 80, t: 130}, movement: 't'},

{'name': 'Hammock Driven Development', pc:{r: 180, t: 170}, movement: 'c'},
{'name': 'Hole Driven Development', pc:{r: 120, t: 175}, movement: 'c'},
{'name': 'Lean', pc:{r: 180, t: 130}, movement: 'c'},

{'name': 'Pair Programming', pc:{r: 280, t: 150}, movement: 't'},
{'name': 'Lean Startup', pc:{r: 290, t: 130}, movement: 'c'},
{'name': 'Systems Thinking', pc:{r: 230, t: 140}, movement: 'c'},

{'name': 'Dependency Injection', pc:{r: 380, t: 120}, movement: 'c'},
{'name': 'Scrum', pc:{r: 310, t: 160}, movement: 'c'},
{'name': 'Iterations', pc:{r: 360, t: 140}, movement: 't'},

        ]
    },

    { "quadrant": "Tools",
        "left": w-200+30,
        "top" : 18,
        "color" : "#587486",
        "items" : [


{ name: 'Git',                      pc: { r: 10,  t: 60 }, movement: 'c' },
{ name: 'Nix',                      pc: { r: 30,  t: 30 }, movement: 't' },
{ name: 'GitPod',                   pc: { r: 40, t: 20 }, movement: 't' },
{ name: 'VIM',                      pc: { r: 50, t: 45 }, movement: 'c' },
{ name: 'Visual Studio Code',       pc: { r: 60, t: 60 }, movement: 'c' },

{ name: 'Azure DevOps',            pc: { r: 140, t: 60 }, movement: 'c' },
{ name: 'Jetbrains Rider',         pc: { r: 140, t: 20 }, movement: 't' },

{ name: 'Docker',   pc: { r: 270, t: 10 }, movement: 'c' },

{ name: 'Ansible',          pc: { r: 350, t: 15 }, movement: 'c' },
{ name: 'Chef',             pc: { r: 380, t: 10 }, movement: 'c' },
{ name: 'Puppet',           pc: { r: 350, t: 15 }, movement: 'c' },
{ name: 'Visual Studio',    pc: { r: 350, t: 10 }, movement: 'c' },

  ]
    },
    { "quadrant": "Platforms",
        "left" : 45,
         "top" : (h/2 + 18),
        "color" : "#DC6F1D",
        "items" : [

{name: 'Bare Metal',    pc: {r: 20, t: 230}, movement: 't'},
{name: 'Terraform',     pc: {r: 40, t: 195}, movement: 't'},
{name: 'Heroku',        pc: {r: 50, t: 230}, movement: 'c'},
{name: 'NixOS',         pc: {r: 70,  t: 245}, movement: 't'},
{name: 'FreeBSD',       pc: {r: 90,  t: 195}, movement: 'c'},
{name: 'GitHub',        pc: {r: 50,  t: 255}, movement: 'c'},
{name: 'GitLab',        pc: {r: 60,  t: 200}, movement: 'c'},


{name: 'Pulumi',        pc: {r: 170, t: 235}, movement: 'c'},

{name: 'GCE',           pc: {r: 220, t: 220}, movement: 'c'},
{name: 'AWS',           pc: {r: 260, t: 195}, movement: 't'},
{name: 'Azure',         pc: {r: 280, t: 210}, movement: 'c'},
{name: 'Kubernetes',    pc: {r: 280, t: 220}, movement: 't'},

{name: 'Ubuntu',        pc: {r: 330, t: 200}, movement: 'c'},
{name: 'z/OS',           pc: {r: 330, t: 225}, movement: 'c'},
{name: 'AIX',           pc: {r: 360, t: 240}, movement: 'c'},
{name: 'RedHat',        pc: {r: 390, t: 250}, movement: 'c'},
{name: 'Solaris',        pc: {r: 390, t: 260}, movement: 'c'},

        ]
    },
    { "quadrant": "Languages",
        "color" : "#B70062",
        "left"  : (w-200+30),
        "top" :   (h/2 + 18),
        "items" : [

{name: 'Haskell',           pc: {r: 20,  t: 300}, movement: 't'},
{name: 'Rust',              pc: {r: 30,  t: 340}, movement: 't'},
{name: 'Erlang',            pc: {r: 55, t: 310}, movement: 'c'},
{name: 'PureScript',        pc: {r: 75, t: 330}, movement: 'c'},
{name: 'OCaml',             pc: {r: 80, t: 340}, movement: 'c'},
{name: 'F#',                pc: {r: 70,  t: 300}, movement: 'c'},
{name: 'C#',                pc: {r: 70,  t: 290}, movement: 'c'},

{name: 'Golang',            pc: {r: 110, t: 300}, movement: 't'},
{name: 'TypeScript',        pc: {r: 125, t: 325}, movement: 't'},
{name: 'elm',               pc: {r: 170, t: 340}, movement: 'c'},

{name: 'WebAssembly',       pc: {r: 250, t: 310}, movement: 'c'},


{name: 'C++',              pc: {r: 300, t: 280}, movement: 'c'},
{name: 'C98',              pc: {r: 330, t: 280}, movement: 'c'},
{name: 'Java',              pc: {r: 330, t: 280}, movement: 'c'},
{name: 'Scala',              pc: {r: 320, t: 310}, movement: 'c'},
{name: 'Ruby',              pc: {r: 350, t: 300}, movement: 'c'},
{name: 'Python',            pc: {r: 330, t: 320}, movement: 'c'},
{name: 'PHP',               pc: {r: 340, t: 340}, movement: 'c'},
{name: 'JavaScript',        pc: {r: 310, t: 350}, movement: 'c'},
        ]
    }
];
