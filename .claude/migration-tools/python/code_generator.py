"""
Python Code Generator for ABAP Migration

Generates Python code templates based on ABAP patterns.
"""

from typing import List, Dict, Optional
from dataclasses import dataclass


@dataclass
class PythonProject:
    """Represents a Python project structure"""
    name: str
    models: List[str]
    services: List[str]
    api_endpoints: List[str]
    tests: List[str]
    requirements: List[str]


class PythonCodeGenerator:
    """Generate Python code from ABAP patterns"""

    @staticmethod
    def generate_sqlalchemy_model(table_name: str, fields: Dict[str, str]) -> str:
        """Generate SQLAlchemy model from ABAP table structure"""
        class_name = ''.join(word.capitalize() for word in table_name.split('_'))

        code = f'''"""
SQLAlchemy model for {table_name}
Migrated from ABAP table {table_name.upper()}
"""

from sqlalchemy import Column, Integer, String, Numeric, Date, Boolean
from sqlalchemy.ext.declarative import declarative_base
from datetime import date, datetime

Base = declarative_base()


class {class_name}(Base):
    """Model for {table_name}"""
    __tablename__ = '{table_name.lower()}'

'''

        for field_name, field_type in fields.items():
            python_type = PythonCodeGenerator._map_field_type(field_type)
            code += f"    {field_name.lower()} = Column({python_type})\n"

        code += f'''
    def __repr__(self):
        return f"<{class_name}(id={{self.id}})>"

    def to_dict(self):
        """Convert model to dictionary"""
        return {{
{chr(10).join(f"            '{field}': self.{field}," for field in fields.keys())}
        }}
'''

        return code

    @staticmethod
    def generate_pydantic_schema(name: str, fields: Dict[str, str]) -> str:
        """Generate Pydantic schema for API validation"""
        class_name = ''.join(word.capitalize() for word in name.split('_'))

        code = f'''"""
Pydantic schemas for {name}
API request/response validation
"""

from pydantic import BaseModel, Field, validator
from typing import Optional, List
from datetime import date, datetime
from decimal import Decimal


class {class_name}Base(BaseModel):
    """Base schema for {name}"""
'''

        for field_name, field_type in fields.items():
            python_type = PythonCodeGenerator._map_pydantic_type(field_type)
            code += f"    {field_name.lower()}: {python_type}\n"

        code += f'''

class {class_name}Create({class_name}Base):
    """Schema for creating {name}"""
    pass


class {class_name}Update(BaseModel):
    """Schema for updating {name} (all fields optional)"""
'''

        for field_name, field_type in fields.items():
            python_type = PythonCodeGenerator._map_pydantic_type(field_type)
            code += f"    {field_name.lower()}: Optional[{python_type}] = None\n"

        code += f'''

class {class_name}Response({class_name}Base):
    """Schema for {name} response"""
    id: int

    class Config:
        from_attributes = True
'''

        return code

    @staticmethod
    def generate_fastapi_crud_endpoint(entity_name: str, model_name: str) -> str:
        """Generate FastAPI CRUD endpoints"""
        route_name = entity_name.lower()
        class_name = ''.join(word.capitalize() for word in entity_name.split('_'))

        code = f'''"""
FastAPI CRUD endpoints for {entity_name}
Migrated from ABAP function modules
"""

from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List

from models import {model_name}
from schemas import {class_name}Create, {class_name}Update, {class_name}Response
from database import get_db

router = APIRouter(
    prefix="/{route_name}",
    tags=["{route_name}"]
)


@router.get("/", response_model=List[{class_name}Response])
async def get_{route_name}(
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(get_db)
):
    """
    Get all {route_name}
    Equivalent to ABAP: SELECT * FROM {entity_name.upper()}
    """
    items = db.query({model_name}).offset(skip).limit(limit).all()
    return items


@router.get("/{{item_id}}", response_model={class_name}Response)
async def get_{route_name}_by_id(
    item_id: int,
    db: Session = Depends(get_db)
):
    """
    Get {route_name} by ID
    Equivalent to ABAP: SELECT SINGLE * FROM {entity_name.upper()} WHERE id = ...
    """
    item = db.query({model_name}).filter({model_name}.id == item_id).first()
    if item is None:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"{class_name} not found"
        )
    return item


@router.post("/", response_model={class_name}Response, status_code=status.HTTP_201_CREATED)
async def create_{route_name}(
    item: {class_name}Create,
    db: Session = Depends(get_db)
):
    """
    Create new {route_name}
    Equivalent to ABAP: INSERT INTO {entity_name.upper()}
    """
    db_item = {model_name}(**item.dict())
    db.add(db_item)
    db.commit()
    db.refresh(db_item)
    return db_item


@router.put("/{{item_id}}", response_model={class_name}Response)
async def update_{route_name}(
    item_id: int,
    item: {class_name}Update,
    db: Session = Depends(get_db)
):
    """
    Update {route_name}
    Equivalent to ABAP: UPDATE {entity_name.upper()} SET ... WHERE id = ...
    """
    db_item = db.query({model_name}).filter({model_name}.id == item_id).first()
    if db_item is None:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"{class_name} not found"
        )

    # Update only provided fields
    update_data = item.dict(exclude_unset=True)
    for field, value in update_data.items():
        setattr(db_item, field, value)

    db.commit()
    db.refresh(db_item)
    return db_item


@router.delete("/{{item_id}}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_{route_name}(
    item_id: int,
    db: Session = Depends(get_db)
):
    """
    Delete {route_name}
    Equivalent to ABAP: DELETE FROM {entity_name.upper()} WHERE id = ...
    """
    db_item = db.query({model_name}).filter({model_name}.id == item_id).first()
    if db_item is None:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"{class_name} not found"
        )

    db.delete(db_item)
    db.commit()
    return None
'''

        return code

    @staticmethod
    def generate_requirements_txt(include_fastapi: bool = True,
                                 include_django: bool = False) -> str:
        """Generate requirements.txt"""
        requirements = [
            "# Database",
            "sqlalchemy>=2.0.0",
            "psycopg2-binary>=2.9.0",
            "",
            "# Validation",
            "pydantic>=2.0.0",
            "",
            "# Data Processing",
            "pandas>=2.0.0",
            "python-dateutil>=2.8.0",
            "",
            "# SAP Connectivity (optional)",
            "# pyrfc>=3.0.0  # Requires SAP NetWeaver RFC SDK",
            "requests>=2.31.0",
            "",
            "# Testing",
            "pytest>=7.4.0",
            "pytest-cov>=4.1.0",
            "pytest-asyncio>=0.21.0",
            "",
            "# Development",
            "black>=23.0.0",
            "flake8>=6.0.0",
            "mypy>=1.4.0",
        ]

        if include_fastapi:
            requirements.extend([
                "",
                "# FastAPI",
                "fastapi>=0.104.0",
                "uvicorn[standard]>=0.24.0",
                "python-multipart>=0.0.6",
            ])

        if include_django:
            requirements.extend([
                "",
                "# Django",
                "django>=4.2.0",
                "djangorestframework>=3.14.0",
            ])

        return '\n'.join(requirements)

    @staticmethod
    def generate_database_config() -> str:
        """Generate database configuration"""
        return '''"""
Database configuration
"""

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, declarative_base
from sqlalchemy.pool import QueuePool
import os

# Database URL from environment
DATABASE_URL = os.getenv(
    "DATABASE_URL",
    "postgresql://user:password@localhost:5432/dbname"
)

# Create engine with connection pooling
engine = create_engine(
    DATABASE_URL,
    poolclass=QueuePool,
    pool_size=10,
    max_overflow=20,
    pool_pre_ping=True,  # Verify connections before using
    echo=False  # Set to True for SQL debugging
)

# Session factory
SessionLocal = sessionmaker(
    autocommit=False,
    autoflush=False,
    bind=engine
)

# Base class for models
Base = declarative_base()


def get_db():
    """
    Database session dependency for FastAPI
    Usage: db: Session = Depends(get_db)
    """
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def init_db():
    """Initialize database (create tables)"""
    Base.metadata.create_all(bind=engine)
'''

    @staticmethod
    def generate_pytest_tests(entity_name: str) -> str:
        """Generate pytest test template"""
        class_name = ''.join(word.capitalize() for word in entity_name.split('_'))

        return f'''"""
Tests for {entity_name}
"""

import pytest
from fastapi.testclient import TestClient
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from main import app
from database import Base, get_db
from models import {class_name}

# Test database
SQLALCHEMY_DATABASE_URL = "sqlite:///./test.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={{"check_same_thread": False}})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


@pytest.fixture
def db_session():
    """Create test database"""
    Base.metadata.create_all(bind=engine)
    db = TestingSessionLocal()
    try:
        yield db
    finally:
        db.close()
        Base.metadata.drop_all(bind=engine)


@pytest.fixture
def client(db_session):
    """Create test client"""
    def override_get_db():
        try:
            yield db_session
        finally:
            pass

    app.dependency_overrides[get_db] = override_get_db
    return TestClient(app)


def test_create_{entity_name}(client):
    """Test creating {entity_name}"""
    response = client.post(
        "/{entity_name}/",
        json={{"name": "Test Item"}}
    )
    assert response.status_code == 201
    data = response.json()
    assert data["name"] == "Test Item"
    assert "id" in data


def test_get_{entity_name}(client, db_session):
    """Test getting {entity_name}"""
    # Create test item
    item = {class_name}(name="Test")
    db_session.add(item)
    db_session.commit()

    # Get item
    response = client.get(f"/{entity_name}/{{item.id}}")
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "Test"


def test_update_{entity_name}(client, db_session):
    """Test updating {entity_name}"""
    # Create test item
    item = {class_name}(name="Original")
    db_session.add(item)
    db_session.commit()

    # Update item
    response = client.put(
        f"/{entity_name}/{{item.id}}",
        json={{"name": "Updated"}}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "Updated"


def test_delete_{entity_name}(client, db_session):
    """Test deleting {entity_name}"""
    # Create test item
    item = {class_name}(name="To Delete")
    db_session.add(item)
    db_session.commit()
    item_id = item.id

    # Delete item
    response = client.delete(f"/{entity_name}/{{item_id}}")
    assert response.status_code == 204

    # Verify deletion
    response = client.get(f"/{entity_name}/{{item_id}}")
    assert response.status_code == 404
'''

    @staticmethod
    def _map_field_type(abap_type: str) -> str:
        """Map ABAP type to SQLAlchemy column type"""
        type_mapping = {
            'i': 'Integer',
            'int8': 'Integer',
            'p': 'Numeric(15, 2)',
            'string': 'String(255)',
            'c': 'String',
            'd': 'Date',
            't': 'String(6)',  # HHMMSS
            'n': 'String',
            'x': 'String',
        }
        return type_mapping.get(abap_type.lower(), 'String')

    @staticmethod
    def _map_pydantic_type(abap_type: str) -> str:
        """Map ABAP type to Pydantic type"""
        type_mapping = {
            'i': 'int',
            'int8': 'int',
            'p': 'Decimal',
            'string': 'str',
            'c': 'str',
            'd': 'date',
            't': 'str',
            'n': 'str',
            'x': 'str',
        }
        return type_mapping.get(abap_type.lower(), 'str')


if __name__ == "__main__":
    # Example: Generate model
    fields = {
        'id': 'i',
        'customer_id': 'n',
        'name': 'string',
        'amount': 'p',
        'created_date': 'd',
    }

    print("=== SQLAlchemy Model ===")
    print(PythonCodeGenerator.generate_sqlalchemy_model('customer', fields))

    print("\n=== Pydantic Schema ===")
    print(PythonCodeGenerator.generate_pydantic_schema('customer', fields))

    print("\n=== FastAPI CRUD ===")
    print(PythonCodeGenerator.generate_fastapi_crud_endpoint('customer', 'Customer'))
