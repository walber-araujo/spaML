# Detectar sistema operacional
OS := $(shell uname)

# Versão do GHC (Haskell Compiler)
GHC_VERSION := 9.6.6  # Altere para a versão desejada do GHC

# Nome do modelo salvo
MODEL_NAME := model_saved

# Pastas do projeto
SRC_DIR := haskell
BUILD_DIR := $(SRC_DIR)/dist-newstyle

# Comandos específicos para Linux ou macOS
ifeq ($(OS),Linux)
  RM = rm -rf
  INSTALL_CMD = sudo apt-get update && sudo apt-get install -y ghc cabal-install
else ifeq ($(OS),Darwin)
  RM = rm -rf
  INSTALL_CMD = brew install ghc cabal-install
endif

# Alvos principais
.PHONY: help config build train classify test clean explain save

# Comando de ajuda
help:
	@echo "Comandos disponíveis:"
	@echo "  make help              - Exibe esta lista de comandos."
	@echo "  make config            - Configura o ambiente inicial (instala dependências)."
	@echo "  make build             - Compila o projeto usando cabal."
	@echo "  make train             - Treina o modelo do zero."
	@echo "  make train LOAD_MODEL=<model> - Treina o modelo carregando dados previamente calculados."
	@echo "  make classify          - Classifica uma mensagem com o modelo."
	@echo "  make test              - Executa os testes configurados no projeto."
	@echo "  make clean             - Remove arquivos gerados (builds, modelos e logs)."
	@echo "  make explain           - Exibe explicações sobre as métricas do modelo."
	@echo "  make save              - Salva o modelo e histórico gerado no treinamento."

# Configurar o ambiente inicial
config:
	@echo "Configurando o ambiente inicial..."
	$(INSTALL_CMD)
	cabal update
	@echo "Configuração concluída."

# Compilar o projeto
build:
	cd $(SRC_DIR) && cabal build --only-dependencies --enable-tests --enable-benchmarks
	@echo "Compilação concluída com sucesso."

# Treinar o modelo do zero
train:
	@echo "Não Implementado""

# Treinar o modelo com um modelo previamente salvo
train-loaded:
	@echo "Não Implementado"

# Classificar uma mensagem
classify:
	@echo "Não Implementado"

# Executar testes
test:
	cd $(SRC_DIR) && cabal test
	@echo "Testes executados."

# Limpar arquivos gerados
clean:
	@echo "Não Implementado"

# Explicar métricas do modelo
explain:
	@echo "Não Implementado"

# Salvar modelo e histórico
save:
	@echo "Não Implementado"