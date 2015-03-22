import StateVar
import Data.Word

type TextureUnit = Int
type GLId = Int
data Program = Program GLId

-- | any GLSL 'uniform' variable
type UniformVar = SettableStateVar
-- | GLSL texture sampler like 'sampler2D'
data UniformSampler dim px = UniformSampler TextureUnit (UniformVar (Maybe (Texture dim px)))

data Tex2D
type UniformSampler2D = UniformSampler Tex2D

-- | Captures the OpenGL object identifier
-- with phantom types for type safety, backed by opengl
-- because format and dim is immutable with 'glTexStorage'
data Texture dim px = Texture GLId
-- | RGBA Color (Byte)
data Color = Color Word8 Word8 Word8 Word8

-- | Material is a combination of a material color and
-- a concrete material 'Texture'
data Material dim px = Material
	{ color 	:: Color
	, texture :: Texture dim px
	}

{- Example GLSL Fragment Shader Code

uniform vec4 			MaterialColor;
uniform int       Time;
uniform sampler2D MaterialTexture;

in 			vec2 			inUV; // from one of the previous stages (e.g. vertex shader)

out vec4 fragColor;

void main ()
{
	fragColor = MaterialColor * texture(MaterialTexture, inUV);
}
-}

-- | GL Program Handle
materialUniform :: Program -> String -> IO (UniformVar (Material dim px))
materialUniform prog varname = do
	colorVar 		<- colorUniform prog (varname ++ "Color")
	textureVar 	<- textureUniform prog (varname ++ "Texture")
	return $ SettableStateVar $ \material -> do
		colorVar   $= color material
		textureVar $= Just (texture material)

colorUniform :: Program -> String -> IO (UniformVar Color)
colorUniform prog varname = undefined -- see implementation "Yage.Uniform.Material" ~5 lines of code

textureUniform :: Program -> String -> IO (UniformVar (Maybe (Texture dim px)))
textureUniform prog varname = undefined -- see implementation "Yage.Uniform.Material" ~5 lines of code

---------------------

data FragmentShader = FragmentShaderInterface
	{ material :: UniformVar (Material dim px) 
	, time     :: UniformVar Integer
	}

